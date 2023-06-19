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
        Bottom | Epsilon | Set(..) => node,
        Concat(old) => {
            let mut new = RcVec::new();
            for x in old {
                let x = normalize(x.clone());
                match x.as_ref() {
                    Bottom => return RegexTree::bottom(), // x ~ bot == bot
                    Epsilon => continue,                  // x ~ eps == x
                    Concat(subvec) => new.extend(subvec.iter().cloned()), // flatten
                    _ => new.push(x.clone()),
                }
            }
            if new.is_empty() {
                RegexTree::epsilon()
            } else if new.len() == 1 {
                new.pop().unwrap()
            } else if sequence_unchanged(&new, old) {
                node
            } else {
                Rc::new(Concat(new))
            }
        }
        Union(old) => {
            let mut new = RcVec::new();
            let mut set = None;
            for x in old {
                let x = normalize(x.clone());
                match x.as_ref() {
                    _ if x == RegexTree::top() => return RegexTree::top(), // x | top == top
                    Bottom => continue,                                    // x | bot == x
                    Union(subvec) => {
                        for y in subvec {
                            match y.as_ref() {
                                Set(subset) => {
                                    set = match set {
                                        None => Some(subset.clone()),
                                        Some(set) => Some(set.union(subset)),
                                    }
                                }
                                _ => new.push(y.clone()),
                            }
                        }
                    }
                    Set(subset) => {
                        set = match set {
                            None => Some(subset.clone()),
                            Some(set) => Some(set.union(subset)),
                        }
                    }
                    _ => new.push(x.clone()),
                }
            }
            if let Some(set) = set {
                new.push(Rc::new(Set(set)));
            }

            new.sort_unstable();
            new.dedup_by(|x, y| Rc::ptr_eq(x, y) || x == y);

            if new
                .iter()
                .any(|x| !matches!(x.as_ref(), Epsilon) && x.is_nullable())
            {
                new.retain(|x| !matches!(x.as_ref(), Epsilon));
            }

            if new.is_empty() {
                RegexTree::bottom()
            } else if new.len() == 1 {
                new.pop().unwrap()
            } else if sequence_unchanged(&new, old) {
                node
            } else {
                Rc::new(Union(new))
            }
        }
        Intersection(old) => {
            let mut new = RcVec::new();
            let mut set = Intervals::full_set();
            for x in old {
                let x = normalize(x.clone());
                match x.as_ref() {
                    Bottom => return RegexTree::bottom(),   // x & bot == bot
                    _ if x == RegexTree::top() => continue, // x & top == x
                    Intersection(subvec) => {
                        for y in subvec {
                            match y.as_ref() {
                                Set(subset) => match set.intersection(subset) {
                                    Some(new_set) => set = new_set,
                                    None => return RegexTree::bottom(),
                                },
                                _ => new.push(y.clone()),
                            }
                        }
                    }
                    Set(subset) => match set.intersection(subset) {
                        Some(new_set) => set = new_set,
                        None => return RegexTree::bottom(),
                    },
                    _ => new.push(x.clone()),
                }
            }
            new.push(Rc::new(Set(set)));

            new.sort_unstable();
            new.dedup_by(|x, y| Rc::ptr_eq(x, y) || x == y);

            if new.is_empty() {
                RegexTree::top()
            } else if new.len() == 1 {
                new.pop().unwrap()
            } else if sequence_unchanged(&new, old) {
                node
            } else {
                Rc::new(Intersection(new))
            }
        }
        KleeneClosure(old) => {
            let new = normalize(old.clone());
            match new.as_ref() {
                KleeneClosure(_) => new,
                Bottom | Epsilon => RegexTree::epsilon(),
                _ => {
                    if Rc::ptr_eq(old, &new) {
                        node
                    } else {
                        Rc::new(KleeneClosure(new))
                    }
                }
            }
        }
        Complement(old) => {
            let new = normalize(old.clone());
            match new.as_ref() {
                Set(x) => match x.complement() {
                    Some(y) => Rc::new(Set(y)),
                    None => RegexTree::bottom(),
                },
                Complement(r) => r.clone(),
                // capture renormalization cases (no need to do allocations)
                _ if Rc::ptr_eq(old, &new) => node,
                _ => Rc::new(Complement(new)),
            }
        }
    }
}
