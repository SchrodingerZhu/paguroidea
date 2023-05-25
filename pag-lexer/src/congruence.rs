// Copyright (c) 2023 Paguroidea Developpers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

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
    result.dedup_by(|x, y| x == y);
    result
}

pub fn approximate_congruence_class(tree: &RegexTree) -> Vec<Intervals> {
    match tree {
        RegexTree::Epsilon => vec![unsafe { intervals!((0, 0x10FFFF)).unwrap_unchecked() }],
        RegexTree::Top => {
            vec![unsafe { intervals!((0, 0x10FFFF)).unwrap_unchecked() }]
        }
        RegexTree::Bottom => {
            vec![unsafe { intervals!((0, 0x10FFFF)).unwrap_unchecked() }]
        }
        RegexTree::Set(x) => {
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
        RegexTree::Concat(r, s) => {
            if !r.is_nullable() {
                approximate_congruence_class(r)
            } else {
                meet(
                    &approximate_congruence_class(r),
                    &approximate_congruence_class(s),
                )
            }
        }
        RegexTree::KleeneClosure(r) => approximate_congruence_class(r),
        RegexTree::Union(r, s) => meet(
            &approximate_congruence_class(r),
            &approximate_congruence_class(s),
        ),
        RegexTree::Intersection(r, s) => meet(
            &approximate_congruence_class(r),
            &approximate_congruence_class(s),
        ),
        RegexTree::Complement(r) => approximate_congruence_class(r),
    }
}
