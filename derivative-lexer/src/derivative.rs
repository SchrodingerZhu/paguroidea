/*
Copyright (C) 2023 Paguroidea Developpers

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at <https://mozilla.org/MPL/2.0/>.
*/

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
