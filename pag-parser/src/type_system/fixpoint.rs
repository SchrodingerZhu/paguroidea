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

type Graph = Vec<Node>;

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
            let Some(&id) = sym_to_id.get(symbol) else { return };
            neighbors.push(id);
        }
        _ => {}
    }
}

fn construct_graph<'src>(binding_ctx: &BindingContext<'src, '_>) -> (Graph, Vec<Symbol<'src>>) {
    let mut sym_to_id = HashMap::new();
    let mut id_to_sym = Vec::new();
    for (idx, (symbol, _)) in binding_ctx.iter().enumerate() {
        sym_to_id.insert(*symbol, idx as NodeId);
        id_to_sym.push(*symbol);
    }

    let mut nodes = Vec::new();
    for (idx, (_, rule)) in binding_ctx.iter().enumerate() {
        let mut neighbors = Vec::new();
        find_neighbors(rule.term, &mut neighbors, &sym_to_id);
        // detect self reference
        let in_cycle = Cell::new(neighbors.iter().any(|id| *id == idx as _));
        nodes.push(Node {
            neighbors,
            in_cycle,
            ..Node::default()
        })
    }

    (nodes, id_to_sym)
}

fn tarjan(node_id: NodeId, dfn_cnt: &mut u32, stack: &mut Vec<NodeId>, graph: &Graph) {
    let node = &graph[node_id as usize];

    *dfn_cnt += 1;
    node.low.set(*dfn_cnt);
    node.dfn.set(*dfn_cnt);
    stack.push(node_id);
    node.in_stack.set(true);

    for &next_id in &node.neighbors {
        let next = &graph[next_id as usize];
        if next.dfn.get() == 0 {
            tarjan(next_id, dfn_cnt, stack, graph);
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
            let top = &graph[top_id as usize];
            top.in_stack.set(false);
            top.in_cycle.set(true);
            if top_id == node_id {
                break;
            }
        }
    }
}

pub fn infer_fixpoints<'src, 'a>(
    arena: &'a TermArena<'src, 'a>,
    binding_ctx: &mut BindingContext<'src, 'a>,
) {
    let (graph, id_to_sym) = construct_graph(binding_ctx);
    let mut dfn_cnt = 0;
    let mut stack = Vec::new();

    for (id, node) in graph.iter().enumerate() {
        if node.dfn.get() == 0 {
            tarjan(id as NodeId, &mut dfn_cnt, &mut stack, &graph);
        }
    }

    for (id, node) in graph.iter().enumerate() {
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
