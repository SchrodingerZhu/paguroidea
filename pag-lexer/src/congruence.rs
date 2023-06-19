// Copyright (c) 2023 Paguroidea Developers
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
            let z = x.intersection(y);
            if !z.is_empty_set() {
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
        Epsilon => vec![intervals!((u8::MIN, u8::MAX))],
        Set(x) => {
            let x = x.clone();
            let y = x.complement();
            if x.is_full_set() {
                vec![x]
            } else {
                vec![x, y]
            }
        }
        Concat(children) => {
            let mut result = approximate_congruence_class(children[0].as_ref());
            for pair in children.windows(2) {
                let [x, y] = pair else { unreachable!() };
                if !x.is_nullable() {
                    return result;
                }
                result = meet(&result, &approximate_congruence_class(y.as_ref()))
            }
            result
        }
        Union(children) | Intersection(children) => children
            .iter()
            .map(|x| approximate_congruence_class(x.as_ref()))
            .reduce(|acc, x| meet(&acc, &x))
            .unwrap(),
        KleeneClosure(r) | Complement(r) => approximate_congruence_class(r),
    }
}
