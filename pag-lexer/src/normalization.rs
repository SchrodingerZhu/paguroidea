// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::regex_tree::RegexTree;
use std::rc::Rc;

pub fn normalize(tree: Rc<RegexTree>) -> Rc<RegexTree> {
    match tree.as_ref() {
        RegexTree::Complement(r) => {
            let r = normalize(r.clone());
            match r.as_ref() {
                RegexTree::Set(x) => match x.complement() {
                    Some(y) => Rc::new(RegexTree::Set(y)),
                    None => Rc::new(RegexTree::Bottom),
                },
                RegexTree::Complement(r) => r.clone(),
                RegexTree::Top => Rc::new(RegexTree::Bottom),
                RegexTree::Bottom => Rc::new(RegexTree::Top),
                _ => Rc::new(RegexTree::Complement(r)),
            }
        }
        RegexTree::Intersection(r, s) => {
            // referential equality
            if Rc::ptr_eq(r, s) {
                return normalize(r.clone());
            }
            let r = normalize(r.clone());
            let s = normalize(s.clone());
            // empty & r = empty
            if matches!(r.as_ref(), RegexTree::Bottom) {
                return r;
            }
            if matches!(s.as_ref(), RegexTree::Bottom) {
                return s;
            }
            // any & r = r
            if matches!(s.as_ref(), RegexTree::Top) {
                return r;
            }
            if matches!(r.as_ref(), RegexTree::Top) {
                return s;
            }

            let ordering = r.cmp(&s);
            if ordering.is_eq() {
                return r;
            }
            if let RegexTree::Set(r) = r.as_ref()
                && let RegexTree::Set(s) = s.as_ref() {
                return match r.intersection(s) {
                    Some(set) => Rc::new(RegexTree::Set(set)),
                    None => Rc::new(RegexTree::Bottom),
                }
            }
            // always right heavy
            if let RegexTree::Intersection(r1, r2) = r.as_ref() {
                return normalize(Rc::new(RegexTree::Intersection(
                    r1.clone(),
                    Rc::new(RegexTree::Intersection(r2.clone(), s)),
                )));
            }

            // will not fall in any of the above cases -- it is safe to return
            if ordering.is_lt() || matches!(s.as_ref(), RegexTree::Intersection(..)) {
                Rc::new(RegexTree::Intersection(r, s))
            } else {
                Rc::new(RegexTree::Intersection(s, r))
            }
        }
        RegexTree::Set(x) => {
            if x.is_full_set() {
                Rc::new(RegexTree::Top)
            } else {
                tree
            }
        }
        RegexTree::Epsilon => tree,
        RegexTree::Concat(r, s) => {
            let nr = normalize(r.clone());
            let ns = normalize(s.clone());
            // empty . s = empty
            if matches!(nr.as_ref(), RegexTree::Bottom) {
                return nr;
            }
            // r . empty = empty
            if matches!(ns.as_ref(), RegexTree::Bottom) {
                return ns;
            }
            // r. epsilon = r
            if matches!(ns.as_ref(), RegexTree::Epsilon) {
                return nr;
            }
            // epsilon . s = s
            if matches!(nr.as_ref(), RegexTree::Epsilon) {
                return ns;
            }
            if let RegexTree::Concat(r1, r2) = nr.as_ref() {
                return normalize(Rc::new(RegexTree::Concat(
                    r1.clone(),
                    Rc::new(RegexTree::Concat(r2.clone(), ns)),
                )));
            }

            if Rc::ptr_eq(&nr, r) && Rc::ptr_eq(&ns, s) {
                return tree;
            }
            Rc::new(RegexTree::Concat(nr, ns))
        }
        RegexTree::KleeneClosure(r) => {
            let nr = normalize(r.clone());
            match nr.as_ref() {
                RegexTree::KleeneClosure(_) => nr,
                RegexTree::Bottom => Rc::new(RegexTree::Epsilon),
                RegexTree::Epsilon => Rc::new(RegexTree::Epsilon),
                _ => {
                    if Rc::ptr_eq(r, &nr) {
                        return tree;
                    }
                    Rc::new(RegexTree::KleeneClosure(nr))
                }
            }
        }
        RegexTree::Union(r, s) => {
            // referential equality
            if Rc::ptr_eq(r, s) {
                return normalize(r.clone());
            }
            let r = normalize(r.clone());
            let s = normalize(s.clone());

            // empty | r = r
            if matches!(r.as_ref(), RegexTree::Bottom) {
                return s;
            }
            if matches!(s.as_ref(), RegexTree::Bottom) {
                return r;
            }
            // any | r = any
            if matches!(s.as_ref(), RegexTree::Top) {
                return s;
            }
            if matches!(r.as_ref(), RegexTree::Top) {
                return r;
            }
            let ordering = r.cmp(&s);
            if ordering.is_eq() {
                return r;
            }
            if let RegexTree::Set(r) = r.as_ref()
                && let RegexTree::Set(s) = s.as_ref() {
                return Rc::new(RegexTree::Set(r.union(s)));
            }

            // distributive
            if let RegexTree::Concat(r1, r2) = r.as_ref() &&
                let RegexTree::Concat(s1, s2) = s.as_ref() {
                if r1 == s1 {
                    return normalize(Rc::new(RegexTree::Concat(
                        r1.clone(),
                        Rc::new(RegexTree::Union(r2.clone(), s2.clone())),
                    )));
                }
                if r2 == s2 {
                    return normalize(Rc::new(RegexTree::Concat(
                        Rc::new(RegexTree::Union(r1.clone(), s1.clone())),
                        r2.clone(),
                    )));
                }
            }

            // always right heavy
            if let RegexTree::Union(r1, r2) = r.as_ref() {
                return normalize(Rc::new(RegexTree::Union(
                    r1.clone(),
                    Rc::new(RegexTree::Union(r2.clone(), s)),
                )));
            }

            // will not fall in any of the above cases -- it is safe to return
            if ordering.is_lt() || matches!(s.as_ref(), RegexTree::Union(..)) {
                Rc::new(RegexTree::Union(r, s))
            } else {
                Rc::new(RegexTree::Union(s, r))
            }
        }
        RegexTree::Top => tree,
        RegexTree::Bottom => tree,
    }
}
