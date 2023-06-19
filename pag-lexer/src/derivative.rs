// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::{normalization::normalize, regex_tree::RegexTree};
use smallvec::smallvec;
use std::rc::Rc;

pub fn derivative(tree: Rc<RegexTree>, x: u8) -> Rc<RegexTree> {
    use RegexTree::*;
    match tree.as_ref() {
        Set(set) => {
            if set.contains(x) {
                RegexTree::epsilon()
            } else {
                RegexTree::bottom()
            }
        }
        Epsilon => RegexTree::bottom(),
        Concat(children) => {
            let head = children[0].clone();
            let tail = normalize(Rc::new(Concat(children[1..].into())));
            let lhs = Rc::new(Concat(smallvec![derivative(head.clone(), x), tail.clone()]));
            if head.is_nullable() {
                Rc::new(Union(smallvec![lhs, derivative(tail, x)]))
            } else {
                lhs
            }
        }
        Union(children) => Rc::new(Union(
            children
                .iter()
                .map(|tree| derivative(tree.clone(), x))
                .collect(),
        )),
        Intersection(children) => Rc::new(Intersection(
            children
                .iter()
                .map(|tree| derivative(tree.clone(), x))
                .collect(),
        )),
        KleeneClosure(r) => Rc::new(Concat(smallvec![derivative(r.clone(), x), tree.clone()])),
        Complement(r) => Rc::new(Complement(derivative(r.clone(), x))),
    }
}
