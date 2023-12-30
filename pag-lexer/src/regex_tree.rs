// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::intervals;
use crate::intervals::Intervals;
use smallvec::SmallVec;
use std::fmt::{Display, Formatter};
use std::ops::RangeInclusive;
use std::rc::Rc;

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone, Hash)]
pub enum RegexTree {
    Bottom, // no character
    Set(Intervals),
    Epsilon,
    Concat(SmallVec<[Rc<Self>; 2]>),
    KleeneClosure(Rc<Self>),
    Union(SmallVec<[Rc<Self>; 2]>),
    Intersection(SmallVec<[Rc<Self>; 2]>),
    Complement(Rc<Self>),
}

use RegexTree::*;

impl Display for RegexTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Bottom => write!(f, "⊥"),
            Concat(x) | Intersection(x) | Union(x) if x.is_empty() => write!(f, "⊥"),
            Set(x) => write!(f, "{x}"),
            Epsilon => write!(f, "ε"),
            Concat(children) => {
                write!(f, "({}", children[0])?;
                for i in &children[1..] {
                    write!(f, " ~ {i}")?;
                }
                write!(f, ")")
            }
            KleeneClosure(x) => write!(f, "{x}*"),
            Union(children) => {
                write!(f, "({}", children[0])?;
                for i in &children[1..] {
                    write!(f, " ∪ {i}")?;
                }
                write!(f, ")")
            }
            Intersection(children) => {
                write!(f, "({}", children[0])?;
                for i in &children[1..] {
                    write!(f, " ∩ {i}")?;
                }
                write!(f, ")")
            }
            Complement(x) => write!(f, "¬{x}"),
        }
    }
}

thread_local! {
    static EPSILON: Rc<RegexTree> = Rc::new(RegexTree::Epsilon);
    static BOTTOM: Rc<RegexTree> = Rc::new(RegexTree::Bottom);
    static TOP: Rc<RegexTree> = BOTTOM.with(|x| Rc::new(RegexTree::Complement(x.clone())));
}

impl RegexTree {
    pub fn epsilon() -> Rc<Self> {
        EPSILON.with(Rc::clone)
    }
    pub fn bottom() -> Rc<Self> {
        BOTTOM.with(Rc::clone)
    }
    pub fn top() -> Rc<Self> {
        TOP.with(Rc::clone)
    }
    pub fn is_byte_sequence(&self) -> bool {
        match self {
            Set(intervals) => intervals.is_single_byte(),
            Concat(children) => children.iter().all(|x| x.is_byte_sequence()),
            Epsilon => true,
            _ => false,
        }
    }
    pub fn as_byte_sequence(&self) -> Option<Vec<u8>> {
        match self {
            Set(intervals) if intervals.is_single_byte() => Some(vec![intervals.representative()]),
            Concat(children) => {
                let init = if let Some(x) = children.first() {
                    x.as_byte_sequence()
                } else {
                    return Some(Vec::new());
                };

                children[1..].iter().fold(init, |acc, x| {
                    acc.and_then(|mut acc| {
                        Some({
                            acc.extend(x.as_byte_sequence()?);
                            acc
                        })
                    })
                })
            }
            Epsilon => Some(Vec::new()),
            _ => None,
        }
    }
    pub fn single(x: u8) -> Self {
        Set(intervals!((x, x)))
    }
    pub fn range(x: RangeInclusive<u8>) -> Self {
        if x.is_empty() {
            return Bottom;
        }
        Set(intervals!((*x.start(), *x.end())))
    }
    pub fn is_nullable(&self) -> bool {
        match self {
            Bottom => false,
            Set(_) => false,
            Epsilon => true,
            Concat(children) | Intersection(children) => children.iter().all(|x| x.is_nullable()),
            KleeneClosure(_) => true,
            Union(children) => children.iter().any(|x| x.is_nullable()),
            Complement(r) => !r.is_nullable(),
        }
    }
}
