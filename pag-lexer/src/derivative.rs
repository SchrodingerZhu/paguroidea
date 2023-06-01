// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::regex_tree::RegexTree;
use std::rc::Rc;

pub fn derivative(tree: Rc<RegexTree>, x: u8) -> RegexTree {
    use RegexTree::*;
    match tree.as_ref() {
        Set(set) => {
            if set.contains(x) {
                Epsilon
            } else {
                Bottom
            }
        }
        Epsilon => Bottom,
        Concat(r, s) => {
            let lhs = Concat(Rc::new(derivative(r.clone(), x)), s.clone());
            if r.is_nullable() {
                Union(Rc::new(lhs), Rc::new(derivative(s.clone(), x)))
            } else {
                lhs
            }
        }
        KleeneClosure(r) => Concat(Rc::new(derivative(r.clone(), x)), tree.clone()),
        Union(r, s) => Union(
            Rc::new(derivative(r.clone(), x)),
            Rc::new(derivative(s.clone(), x)),
        ),
        Intersection(r, s) => Intersection(
            Rc::new(derivative(r.clone(), x)),
            Rc::new(derivative(s.clone(), x)),
        ),
        Complement(r) => Complement(Rc::new(derivative(r.clone(), x))),
        Top => Epsilon,
        Bottom => Bottom,
    }
}
