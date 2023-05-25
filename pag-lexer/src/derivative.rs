// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::regex_tree::RegexTree;
use std::rc::Rc;

pub fn derivative(tree: Rc<RegexTree>, x: u32) -> RegexTree {
    match tree.as_ref() {
        RegexTree::Set(set) => {
            if set.contains(x) {
                RegexTree::Epsilon
            } else {
                RegexTree::Bottom
            }
        }
        RegexTree::Epsilon => RegexTree::Bottom,
        RegexTree::Concat(r, s) => {
            let lhs = RegexTree::Concat(Rc::new(derivative(r.clone(), x)), s.clone());
            if r.is_nullable() {
                RegexTree::Union(Rc::new(lhs), Rc::new(derivative(s.clone(), x)))
            } else {
                lhs
            }
        }
        RegexTree::KleeneClosure(r) => {
            RegexTree::Concat(Rc::new(derivative(r.clone(), x)), tree.clone())
        }
        RegexTree::Union(r, s) => RegexTree::Union(
            Rc::new(derivative(r.clone(), x)),
            Rc::new(derivative(s.clone(), x)),
        ),
        RegexTree::Intersection(r, s) => RegexTree::Intersection(
            Rc::new(derivative(r.clone(), x)),
            Rc::new(derivative(s.clone(), x)),
        ),
        RegexTree::Complement(r) => RegexTree::Complement(Rc::new(derivative(r.clone(), x))),
        RegexTree::Top => RegexTree::Epsilon,
        RegexTree::Bottom => RegexTree::Bottom,
    }
}
