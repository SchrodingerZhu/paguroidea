// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::{intervals::Intervals, regex_tree::RegexTree};
use smallvec::{smallvec, SmallVec};
use std::rc::Rc;
use RegexTree::*;

type RcVec = SmallVec<[Rc<RegexTree>; 2]>;

macro_rules! recursive_flatten {
    ($name:ident, $ctor:ident) => {
        fn $name(nodes: &[Rc<RegexTree>]) -> RcVec {
            nodes
                .iter()
                .flat_map(|x| match x.as_ref() {
                    $ctor(inner) => $name(&inner),
                    _ => smallvec![x.clone()],
                })
                .collect()
        }
    };
}

recursive_flatten! {
    flatten_concat, Concat
}

recursive_flatten! {
    flatten_union, Union
}

recursive_flatten! {
    flatten_intersection, Intersection
}

fn sequence_unchanged(x: &[Rc<RegexTree>], y: &[Rc<RegexTree>]) -> bool {
    x.iter()
        .map(|x| x.as_ref() as *const RegexTree)
        .eq(y.iter().map(|y| y.as_ref() as *const RegexTree))
}

pub fn normalize(node: Rc<RegexTree>) -> Rc<RegexTree> {
    match node.as_ref() {
        Bottom | Epsilon | Set(..) => node,
        Concat(old) => {
            let new: RcVec = old.iter().map(|x| normalize(x.clone())).collect();
            let new: RcVec = flatten_concat(&new);
            if new.iter().any(|x| matches!(x.as_ref(), Bottom)) {
                return RegexTree::bottom();
            }
            let new: RcVec = new
                .into_iter()
                .filter(|x| !matches!(x.as_ref(), Epsilon))
                .collect();
            if new.len() == 1 {
                new[0].clone()
            } else if new.is_empty() {
                RegexTree::epsilon()
            } else if sequence_unchanged(&new, old) {
                node
            } else {
                Rc::new(Concat(new))
            }
        }
        KleeneClosure(old) => {
            let new = normalize(old.clone());
            match &*new {
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
        Union(old) => {
            let new: RcVec = old.iter().map(|x| normalize(x.clone())).collect();
            let new: RcVec = flatten_union(&new);
            if new.iter().any(|x| x == &RegexTree::top()) {
                return RegexTree::top();
            }
            let mut sets = None;
            let mut nonsets = Vec::new();

            for i in new {
                if let Set(x) = i.as_ref() {
                    sets = match sets {
                        None => Some(x.clone()),
                        Some(y) => Some(x.union(&y)),
                    }
                } else {
                    nonsets.push(i);
                }
            }

            let mut new: RcVec = sets
                .map(|x| Rc::new(Set(x)))
                .into_iter()
                .chain(nonsets.into_iter())
                .collect();

            new.sort_unstable();
            new.dedup_by(|x, y| Rc::ptr_eq(x, y) || x == y);
            new = new
                .into_iter()
                .filter(|x| !matches!(x.as_ref(), Bottom))
                .collect();

            if new
                .iter()
                .any(|x| !matches!(x.as_ref(), Epsilon) && x.is_nullable())
            {
                new = new
                    .into_iter()
                    .filter(|x| !matches!(x.as_ref(), Epsilon))
                    .collect();
            }

            if new.len() == 1 {
                return new[0].clone();
            }

            if new.is_empty() {
                return RegexTree::bottom();
            }

            if sequence_unchanged(&new, old) {
                node
            } else {
                Rc::new(Union(new))
            }
        }
        Intersection(old) => {
            let new: RcVec = old.iter().map(|x| normalize(x.clone())).collect();
            let new: RcVec = flatten_intersection(&new);
            if new.iter().any(|x| matches!(x.as_ref(), Bottom)) {
                return RegexTree::bottom();
            }
            let new: RcVec = new.into_iter().filter(|x| x != &RegexTree::top()).collect();
            let mut sets = Some(Intervals::full_set());
            let mut nonsets = Vec::new();

            for i in new {
                if let Set(x) = i.as_ref() {
                    sets = match sets {
                        None => None,
                        Some(y) => x.intersection(&y),
                    }
                } else {
                    nonsets.push(i);
                }
            }

            let mut new: RcVec = match sets {
                None => return RegexTree::bottom(),
                Some(x) => [Rc::new(Set(x))]
                    .into_iter()
                    .chain(nonsets.into_iter())
                    .collect(),
            };

            new.sort_unstable();
            new.dedup_by(|x, y| Rc::ptr_eq(x, y) || x == y);

            if new.len() == 1 {
                return new[0].clone();
            }

            if new.is_empty() {
                return RegexTree::top();
            }

            if sequence_unchanged(&new, old) {
                node
            } else {
                Rc::new(Intersection(new))
            }
        }
        Complement(old) => {
            let r = normalize(old.clone());
            match &*r {
                Set(x) => match x.complement() {
                    Some(y) => Rc::new(Set(y)),
                    None => RegexTree::bottom(),
                },
                Complement(r) => r.clone(),
                // capture renormalization cases (no need to do allocations)
                _ if Rc::ptr_eq(old, &r) => node,
                _ => Rc::new(Complement(r)),
            }
        }
    }
}
