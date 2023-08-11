// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

// Modified from Tarjan's strongly connected components algorithm

use std::cell::Cell;
use std::collections::HashMap;

use crate::core_syntax::{BindingContext, Term, TermArena, TermPtr};
use crate::frontend::WithSpan;
use crate::utilities::Symbol;

type NodeId = u32;

#[derive(Default)]
struct Node {
    neighbors: Vec<NodeId>,
    in_stack: Cell<bool>,
    low: Cell<u32>,
    dfn: Cell<u32>,
    in_cycle: Cell<bool>, // scc size > 1 or self reference
}

fn find_neighbors(
    term: TermPtr,
    neighbors: &mut Vec<NodeId>,
    sym_to_id: &HashMap<Symbol<'_>, NodeId>,
) {
    match &term.node {
        Term::Sequence(lhs, rhs) | Term::Alternative(lhs, rhs) => {
            find_neighbors(lhs, neighbors, sym_to_id);
            find_neighbors(rhs, neighbors, sym_to_id);
        }
        Term::Fix(_, expr) => find_neighbors(expr, neighbors, sym_to_id),
        Term::ParserRef(symbol) => {
            // unexisted IDs refer to implicit fixpoints
            let Some(&id) = sym_to_id.get(symbol) else {
                return;
            };
            neighbors.push(id);
        }
        _ => {}
    }
}

fn tarjan(node_id: NodeId, dfn_cnt: &mut u32, stack: &mut Vec<NodeId>, nodes: &Vec<Node>) {
    let node = &nodes[node_id as usize];

    *dfn_cnt += 1;
    node.low.set(*dfn_cnt);
    node.dfn.set(*dfn_cnt);
    stack.push(node_id);
    node.in_stack.set(true);

    for &next_id in &node.neighbors {
        // self reference
        if next_id == node_id {
            node.in_cycle.set(true);
            continue;
        }
        let next = &nodes[next_id as usize];
        if next.dfn.get() == 0 {
            tarjan(next_id, dfn_cnt, stack, nodes);
            node.low.set(node.low.get().min(next.low.get())); // u.low = min(u.low, v.low)
        } else if next.in_stack.get() {
            node.low.set(node.low.get().min(next.dfn.get())); // u.low = min(u.low, v.dfn)
        }
    }

    if node.low.get() == node.dfn.get() {
        // scc size == 1
        if stack.last() == Some(&node_id) {
            node.in_stack.set(false);
            stack.pop();
            return;
        }
        // scc size > 1
        while let Some(top_id) = stack.pop() {
            let top = &nodes[top_id as usize];
            top.in_stack.set(false);
            top.in_cycle.set(true);
            if top_id == node_id {
                break;
            }
        }
    }
}

pub fn infer_fixpoints<'src, 'arena>(
    entrypoint: Symbol<'src>,
    arena: &'arena TermArena<'src, 'arena>,
    binding_ctx: &mut BindingContext<'src, 'arena>,
) {
    let mut sym_to_id = HashMap::new();
    let mut id_to_sym = Vec::new();
    for (idx, (symbol, _)) in binding_ctx.iter().enumerate() {
        sym_to_id.insert(*symbol, idx as NodeId);
        id_to_sym.push(*symbol);
    }

    let mut nodes = Vec::new();
    for (_, rule) in binding_ctx.iter() {
        let mut neighbors = Vec::new();
        find_neighbors(rule.term, &mut neighbors, &sym_to_id);
        nodes.push(Node {
            neighbors,
            ..Node::default()
        })
    }

    let begin = sym_to_id[&entrypoint] as NodeId;
    let mut dfn_cnt = 0;
    let mut stack = Vec::new();
    tarjan(begin, &mut dfn_cnt, &mut stack, &nodes);

    for (id, node) in nodes.iter().enumerate() {
        // unreachable rules
        if node.dfn.get() == 0 {
            let symbol = id_to_sym[id];
            binding_ctx.remove(&symbol);
            continue;
        }
        // fixpoints
        if node.in_cycle.get() {
            let symbol = id_to_sym[id];
            let rule = binding_ctx.get_mut(&symbol).unwrap();
            rule.term = arena.alloc(WithSpan {
                span: rule.term.span,
                node: Term::Fix(symbol, rule.term),
            })
        }
    }
}
