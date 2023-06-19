// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::{intervals::Intervals, regex_tree::RegexTree};
use smallvec::SmallVec;
use std::rc::Rc;
use RegexTree::*;

type RcVec = SmallVec<[Rc<RegexTree>; 2]>;

fn sequence_unchanged(x: &[Rc<RegexTree>], y: &[Rc<RegexTree>]) -> bool {
    x.iter()
        .map(|x| x.as_ref() as *const RegexTree)
        .eq(y.iter().map(|y| y.as_ref() as *const RegexTree))
}

pub fn normalize(node: Rc<RegexTree>) -> Rc<RegexTree> {
    match node.as_ref() {
        Epsilon | Set(_) => node,
        Concat(old) => {
            let mut new = RcVec::new();
            for x in old {
                let x = normalize(x.clone());
                match x.as_ref() {
                    _ if x.is_bottom() => return RegexTree::bottom(), // x ~ bot == bot
                    Epsilon => continue,                              // x ~ eps == x
                    Concat(subvec) => new.extend(subvec.iter().cloned()), // flatten
                    _ => new.push(x.clone()),
                }
            }
            match new.as_slice() {
                [] => RegexTree::epsilon(),
                [_] => new.pop().unwrap(),
                _ if sequence_unchanged(&new, old) => node,
                _ => Rc::new(Concat(new)),
            }
        }
        Union(old) => {
            let mut new = RcVec::new();
            let mut set = Intervals::empty_set();
            for x in old {
                let x = normalize(x.clone());
                match x.as_ref() {
                    Union(subvec) => {
                        for y in subvec {
                            match y.as_ref() {
                                Set(subset) => set = set.union(subset),
                                _ => new.push(y.clone()),
                            }
                        }
                    }
                    Set(subset) => set = set.union(subset),
                    _ => new.push(x.clone()),
                }
            }
            // consider early return and skip?
            if set.is_full_set() {
                return RegexTree::top();
            }
            if !set.is_empty_set() {
                new.push(Rc::new(Set(set)));
            }

            new.sort_unstable();
            new.dedup_by(|x, y| Rc::ptr_eq(x, y) || x == y);

            // temporary workaround for (x* | eps)
            if new
                .iter()
                .any(|x| !matches!(x.as_ref(), Epsilon) && x.is_nullable())
            {
                new.retain(|x| !matches!(x.as_ref(), Epsilon));
            }

            match new.as_slice() {
                [] => RegexTree::bottom(),
                [_] => new.pop().unwrap(),
                _ if sequence_unchanged(&new, old) => node,
                _ => Rc::new(Union(new)),
            }
        }
        Intersection(old) => {
            let mut new = RcVec::new();
            let mut set = Intervals::full_set();
            for x in old {
                let x = normalize(x.clone());
                match x.as_ref() {
                    Intersection(subvec) => {
                        for y in subvec {
                            match y.as_ref() {
                                Set(subset) => set = set.intersection(subset),
                                _ => new.push(y.clone()),
                            }
                        }
                    }
                    Set(subset) => set = set.intersection(subset),
                    _ => new.push(x.clone()),
                }
            }
            // consider early return and skip?
            if set.is_empty_set() {
                return RegexTree::bottom();
            }
            if !set.is_full_set() {
                new.push(Rc::new(Set(set)));
            }

            new.sort_unstable();
            new.dedup_by(|x, y| Rc::ptr_eq(x, y) || x == y);

            match new.as_slice() {
                [] => RegexTree::top(),
                [_] => new.pop().unwrap(),
                _ if sequence_unchanged(&new, old) => node,
                _ => Rc::new(Intersection(new)),
            }
        }
        KleeneClosure(old) => {
            let new = normalize(old.clone());
            match new.as_ref() {
                KleeneClosure(_) => new,
                Epsilon => RegexTree::epsilon(),
                _ if new.is_bottom() => RegexTree::epsilon(),
                _ if Rc::ptr_eq(old, &new) => node,
                _ => Rc::new(KleeneClosure(new)),
            }
        }
        Complement(old) => {
            let new = normalize(old.clone());
            match new.as_ref() {
                Set(x) => Rc::new(Set(x.complement())),
                Complement(r) => r.clone(),
                // capture renormalization cases (no need to do allocations)
                _ if Rc::ptr_eq(old, &new) => node,
                _ => Rc::new(Complement(new)),
            }
        }
    }
}
