// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::regex_tree::RegexTree;
use std::rc::Rc;

pub fn derivative(tree: Rc<RegexTree>, x: u8) -> Rc<RegexTree> {
    use RegexTree::*;
    match tree.as_ref() {
        Set(set) => {
            if set.contains(x) {
                Rc::new(Epsilon)
            } else {
                Rc::new(Bottom)
            }
        }
        Concat(r, s) => {
            let lhs = Rc::new(Concat(derivative(r.clone(), x), s.clone()));
            if r.is_nullable() {
                Rc::new(Union(lhs, derivative(s.clone(), x)))
            } else {
                lhs
            }
        }
        KleeneClosure(r) => Rc::new(Concat(derivative(r.clone(), x), tree.clone())),
        Union(r, s) => Rc::new(Union(derivative(r.clone(), x), derivative(s.clone(), x))),
        Intersection(r, s) => Rc::new(Intersection(
            derivative(r.clone(), x),
            derivative(s.clone(), x),
        )),
        Complement(r) => Rc::new(Complement(derivative(r.clone(), x))),
        Top => Rc::new(Epsilon),
        Bottom | Epsilon => Rc::new(Bottom),
    }
}
