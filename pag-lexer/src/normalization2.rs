// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::regex_tree2::RegexTree;
use smallvec::{smallvec, SmallVec};
use typed_arena::Arena;
use RegexTree::*;

type RefVec<'a> = SmallVec<[&'a RegexTree<'a>; 2]>;
macro_rules! recursive_flatten {
    ($name:ident, $ctor:ident) => {
        fn $name<'a>(nodes: &[&'a RegexTree<'a>]) -> RefVec<'a> {
            nodes
                .iter()
                .flat_map(|x| match x {
                    Concat(inner) => $name(&inner),
                    _ => smallvec![*x],
                })
                .collect()
        }
    };
}

recursive_flatten! {
    flatten_concat, Concat
}

recursive_flatten! {
    flatten_union, Union
}

recursive_flatten! {
    flatten_intersection, Intersection
}

fn sequence_unchanged<'a>(x: &[&'a RegexTree<'a>], y: &[&'a RegexTree<'a>]) -> bool {
    x.iter().map(|x|*x as *const RegexTree<'a>)
        .eq(y.iter().map(|y|*y as *const RegexTree<'a>))
}

fn node_unchanged<'a>(x: &RegexTree<'a>, y: &RegexTree<'a>) -> bool {
    std::ptr::eq(x as *const RegexTree<'a>, y as *const RegexTree<'a>)
}

fn flatten_node<'a>(node: &'a RegexTree<'a>, arena: &'a Arena<RegexTree<'a>>) -> &'a RegexTree<'a> {
    match node {
        Concat(nodes) => {
            let flatten = flatten_concat(nodes);
            if sequence_unchanged(&flatten, nodes) {
                return node;
            }
            &*arena.alloc(Concat(flatten))
        }
        Union(nodes) => {
            let mut flatten = flatten_union(nodes);
            flatten.sort_unstable();
            flatten.dedup();
            if sequence_unchanged(&flatten, nodes) {
                return node;
            }
            &*arena.alloc(Union(flatten))
        },
        Intersection(nodes) => {
            let mut flatten = flatten_intersection(nodes);
            if sequence_unchanged(&flatten, nodes) {
                return node;
            }
            &*arena.alloc(Intersection(flatten))
        },
        _ => return node,
    }
}

fn normalize<'a>(node: &'a RegexTree<'a>, arena: &'a Arena<RegexTree<'a>>) -> &'a RegexTree<'a> {
    match node {
        Top | Bottom | Epsilon => node,
        Set(x) => {
            if x.is_full_set() {
                RegexTree::top()
            } else {
                node
            }
        },
        Concat(_) => todo!(),
        KleeneClosure(_) => todo!(),
        Union(_) => todo!(),
        Intersection(_) => todo!(),
        Complement(old) => {
            let r = normalize(old, arena);
            match &*r {
                Set(x) => match x.complement() {
                    Some(y) => &*arena.alloc(Set(y)),
                    None => RegexTree::bottom(),
                },
                Complement(r) => r,
                Top => RegexTree::bottom(),
                Bottom => RegexTree::top(),
                // capture renormalization cases (no need to do allocations)
                _ if node_unchanged(r, old) => node,
                _ => &*arena.alloc(Complement(r)),
            }
        },
    }
}
