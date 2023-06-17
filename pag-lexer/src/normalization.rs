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
    use RegexTree::*;
    match &*tree {
        Epsilon | Top | Bottom => tree,
        Complement(r) => {
            let r = normalize(r.clone());
            match &*r {
                Set(x) => match x.complement() {
                    Some(y) => Rc::new(Set(y)),
                    None => Rc::new(Bottom),
                },
                Complement(r) => r.clone(),
                Top => Rc::new(Bottom),
                Bottom => Rc::new(Top),
                _ => Rc::new(Complement(r)),
            }
        }
        Intersection(r, s) => {
            // referential equality
            if Rc::ptr_eq(r, s) {
                return normalize(r.clone());
            }
            let r = normalize(r.clone());
            let s = normalize(s.clone());
            // empty & r = empty
            if matches!(&*r, Bottom) {
                return r;
            }
            if matches!(&*s, Bottom) {
                return s;
            }
            // any & r = r
            if matches!(&*s, Top) {
                return r;
            }
            if matches!(&*r, Top) {
                return s;
            }

            let ordering = r.cmp(&s);
            if ordering.is_eq() {
                return r;
            }
            if let (Set(r), Set(s)) = (&*r, &*s) {
                return match r.intersection(s) {
                    Some(set) => Rc::new(Set(set)),
                    None => Rc::new(Bottom),
                };
            }
            // always right heavy
            if let Intersection(r1, r2) = &*r {
                return normalize(Rc::new(Intersection(
                    r1.clone(),
                    Rc::new(Intersection(r2.clone(), s)),
                )));
            }

            // will not fall in any of the above cases -- it is safe to return
            if ordering.is_lt() || matches!(&*s, Intersection(..)) {
                Rc::new(Intersection(r, s))
            } else {
                Rc::new(Intersection(s, r))
            }
        }
        Set(x) => {
            if x.is_full_set() {
                Rc::new(Top)
            } else {
                tree
            }
        }
        Concat(r, s) => {
            let nr = normalize(r.clone());
            let ns = normalize(s.clone());
            // empty . s = empty
            if matches!(&*nr, Bottom) {
                return nr;
            }
            // r . empty = empty
            if matches!(&*ns, Bottom) {
                return ns;
            }
            // r. epsilon = r
            if matches!(&*ns, Epsilon) {
                return nr;
            }
            // epsilon . s = s
            if matches!(&*nr, Epsilon) {
                return ns;
            }
            if let Concat(r1, r2) = &*nr {
                return normalize(Rc::new(Concat(r1.clone(), Rc::new(Concat(r2.clone(), ns)))));
            }

            if Rc::ptr_eq(&nr, r) && Rc::ptr_eq(&ns, s) {
                return tree;
            }
            Rc::new(Concat(nr, ns))
        }
        KleeneClosure(r) => {
            let nr = normalize(r.clone());
            match &*nr {
                KleeneClosure(_) => nr,
                Bottom => Rc::new(Epsilon),
                Epsilon => Rc::new(Epsilon),
                _ => {
                    if Rc::ptr_eq(r, &nr) {
                        return tree;
                    }
                    Rc::new(KleeneClosure(nr))
                }
            }
        }
        Union(r, s) => {
            // referential equality
            if Rc::ptr_eq(r, s) {
                return normalize(r.clone());
            }
            let r = normalize(r.clone());
            let s = normalize(s.clone());

            // empty | r = r
            if matches!(&*r, Bottom) {
                return s;
            }
            if matches!(&*s, Bottom) {
                return r;
            }
            // any | r = any
            if matches!(&*s, Top) {
                return s;
            }
            if matches!(&*r, Top) {
                return r;
            }
            let ordering = r.cmp(&s);
            if ordering.is_eq() {
                return r;
            }
            if let (Set(r), Set(s)) = (&*r, &*s) {
                return Rc::new(Set(r.union(s)));
            }

            // distributive
            if let (Concat(r1, r2), Concat(s1, s2)) = (&*r, &*s) {
                if r1 == s1 {
                    return normalize(Rc::new(Concat(
                        r1.clone(),
                        Rc::new(Union(r2.clone(), s2.clone())),
                    )));
                }
                if r2 == s2 {
                    return normalize(Rc::new(Concat(
                        Rc::new(Union(r1.clone(), s1.clone())),
                        r2.clone(),
                    )));
                }
            }

            // always right heavy
            if let Union(r1, r2) = &*r {
                return normalize(Rc::new(Union(r1.clone(), Rc::new(Union(r2.clone(), s)))));
            }

            if s.contains_in_union(&*r) {
                return s;
            }

            // will not fall in any of the above cases -- it is safe to return
            if ordering.is_lt() || matches!(&*s, Union(..)) {
                Rc::new(Union(r, s))
            } else {
                Rc::new(Union(s, r))
            }
        }
    }
}
