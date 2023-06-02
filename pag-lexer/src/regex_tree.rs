// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::intervals;
use crate::intervals::Intervals;
use std::fmt::{Display, Formatter};
use std::ops::RangeInclusive;
use std::rc::Rc;

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone, Hash)]
pub enum RegexTree {
    Top,
    // any character
    Bottom,
    // no character
    Set(Intervals),
    Epsilon,
    Concat(Rc<RegexTree>, Rc<RegexTree>),
    KleeneClosure(Rc<RegexTree>),
    Union(Rc<RegexTree>, Rc<RegexTree>),
    Intersection(Rc<RegexTree>, Rc<RegexTree>),
    Complement(Rc<RegexTree>),
}

use RegexTree::*;

impl Display for RegexTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Top => write!(f, "⊤"),
            Bottom => write!(f, "⊥"),
            Set(x) => write!(f, "{x}"),
            Epsilon => write!(f, "ε"),
            Concat(x, y) => write!(f, "({x} ~ {y})"),
            KleeneClosure(x) => write!(f, "{x}*"),
            Union(x, y) => write!(f, "({x} ∪ {y})"),
            Intersection(x, y) => write!(f, "({x} ∩ {y})"),
            Complement(x) => write!(f, "¬({x})"),
        }
    }
}

impl RegexTree {
    pub fn is_byte_sequence(&self) -> bool {
        match self {
            Set(intervals) => intervals.is_single_byte(),
            Concat(a, b) => a.is_byte_sequence() && b.is_byte_sequence(),
            Epsilon => true,
            _ => false,
        }
    }
    pub fn as_byte_sequence(&self) -> Option<Vec<u8>> {
        match self {
            Set(intervals) if intervals.is_single_byte() => {
                Some([intervals.representative()].into())
            }
            Concat(a, b) => {
                let mut a = a.as_byte_sequence()?;
                let b = b.as_byte_sequence()?;
                a.extend(b);
                Some(a)
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
    pub fn mangle(&self) -> String {
        match self {
            Top => "T".to_string(),
            Bottom => "B".to_string(),
            Set(x) => x.mangle(),
            Epsilon => "E".to_string(),
            Concat(x, y) => {
                let x = x.mangle();
                let y = y.mangle();
                format!("C{}{}{}{}", x.len(), x, y.len(), y)
            }
            KleeneClosure(x) => {
                let x = x.mangle();
                format!("K{}{}", x.len(), x)
            }
            Union(x, y) => {
                let x = x.mangle();
                let y = y.mangle();
                format!("U{}{}{}{}", x.len(), x, y.len(), y)
            }
            Intersection(x, y) => {
                let x = x.mangle();
                let y = y.mangle();
                format!("I{}{}{}{}", x.len(), x, y.len(), y)
            }
            Complement(x) => {
                let x = x.mangle();
                format!("N{}{}", x.len(), x)
            }
        }
    }
    pub fn is_nullable(&self) -> bool {
        match self {
            Top => false,
            Bottom => false,
            Set(_) => false,
            Epsilon => true,
            Concat(left, right) => left.is_nullable() && right.is_nullable(),
            KleeneClosure(_) => true,
            Union(left, right) => left.is_nullable() || right.is_nullable(),
            Intersection(left, right) => left.is_nullable() && right.is_nullable(),
            Complement(r) => !r.is_nullable(),
        }
    }
}
