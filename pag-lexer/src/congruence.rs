// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::ops::ControlFlow;

use crate::intervals;
use crate::intervals::Intervals;
use crate::regex_tree::RegexTree;

pub fn meet(a: &[Intervals], b: &[Intervals]) -> Vec<Intervals> {
    let mut result = Vec::new();
    for x in a {
        for y in b {
            if let Some(z) = x.intersection(y) {
                result.push(z);
            }
        }
    }
    result.sort();
    result.dedup();
    result
}

// TODO: this part can be optimized
pub fn approximate_congruence_class(tree: &RegexTree) -> Vec<Intervals> {
    use RegexTree::*;
    match tree {
        Epsilon | Bottom => vec![intervals!((0, u8::MAX))],
        Set(x) => {
            let x = x.clone();
            match x.complement() {
                Some(y) => {
                    if x < y {
                        vec![x, y]
                    } else {
                        vec![y, x]
                    }
                }
                None => vec![x],
            }
        }
        Concat(children) => {
            match children[1..]
                .iter()
                .zip(children.iter().map(|x| x.is_nullable()))
                .try_fold(
                    approximate_congruence_class(&children[0]),
                    |acc, (tree, prev_nullable)| {
                        if !prev_nullable {
                            ControlFlow::Break(acc)
                        } else {
                            ControlFlow::Continue(meet(&acc, &approximate_congruence_class(tree)))
                        }
                    },
                ) {
                ControlFlow::Break(v) | ControlFlow::Continue(v) => v,
            }
        }
        KleeneClosure(r) | Complement(r) => approximate_congruence_class(r),
        Union(children) | Intersection(children) => children[1..]
            .iter()
            .fold(approximate_congruence_class(&children[0]), |acc, x| {
                meet(&acc, &approximate_congruence_class(x))
            }),
    }
}
