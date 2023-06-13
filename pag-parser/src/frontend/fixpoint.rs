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

use super::{
    SurfaceSyntaxTree::{self, *},
    WithSpan,
};

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

fn find_neighbors<'src>(
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
    neighbors: &mut Vec<NodeId>,
    id_table: &HashMap<&'src str, NodeId>,
) {
    match &sst.node {
        ParserAlternative { lhs, rhs } | ParserSequence { lhs, rhs } => {
            find_neighbors(lhs, neighbors, id_table);
            find_neighbors(rhs, neighbors, id_table);
        }
        ParserStar { inner } | ParserPlus { inner } | ParserOptional { inner } => {
            find_neighbors(inner, neighbors, id_table);
        }
        ParserRuleRef { name } => {
            // TODO: error handling
            neighbors.push(*id_table.get(name.span.as_str()).unwrap());
        }
        _ => {}
    }
}

fn construct_graph<'src>(sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>) -> Graph {
    let ParserDef { rules, .. } = &sst.node else {
        unreachable!("sst should be a parser definition")
    };

    let mut id_table = HashMap::new();
    for (idx, rule) in rules.iter().enumerate() {
        let ParserRuleDef { name, .. } = &rule.node else {
            unreachable!("parser should only contain rule definitions")
        };
        id_table.insert(name.span.as_str(), idx as NodeId);
    }

    let mut nodes = Vec::new();
    for (idx, rule) in rules.iter().enumerate() {
        let ParserRuleDef { expr, .. } = &rule.node else {
            unreachable!("parser should only contain rule definitions")
        };
        let mut neighbors = Vec::new();
        find_neighbors(&expr, &mut neighbors, &id_table);
        // self reference
        let in_cycle = Cell::new(neighbors.iter().find(|id| **id == idx as _).is_some());
        nodes.push(Node {
            neighbors,
            in_cycle,
            ..Node::default()
        })
    }

    nodes
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
            node.low.set(node.low.get().min(next.low.get()))
        } else if next.in_stack.get() {
            node.low.set(node.low.get().min(next.dfn.get()))
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

pub fn infer_fixpoints<'src>(sst: &mut WithSpan<'src, SurfaceSyntaxTree<'src>>) {
    let graph = construct_graph(sst);
    let mut dfn_cnt = 0;
    let mut stack = Vec::new();

    for (id, node) in graph.iter().enumerate() {
        if node.dfn.get() == 0 {
            tarjan(id as NodeId, &mut dfn_cnt, &mut stack, &graph);
        }
    }

    let ParserDef { rules, .. } = &mut sst.node else {
        unreachable!("sst should be a parser definition")
    };
    for (id, node) in graph.iter().enumerate() {
        if node.in_cycle.get() {
            let ParserRuleDef { fixpoint, .. } = &mut rules[id].node else {
                unreachable!("parser should only contain rule definitions")
            };
            *fixpoint = true;
        }
    }
}
