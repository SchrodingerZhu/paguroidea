// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use smallvec::{smallvec, SmallVec};
use std::ascii::escape_default;
use std::fmt::{Display, Formatter};

#[macro_export]
macro_rules! interval {
    ($start:expr, $end:expr) => {
        $crate::intervals::Interval::new($start as u8, $end as u8)
    };
}

#[macro_export]
macro_rules! intervals {
    ($(($start:expr, $end:expr)),*) => {
        $crate::intervals::Intervals::new(
            vec![$($crate::interval!($start, $end)),*]
        )
    };
}

// A closed interval of u8s.
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
pub struct Interval(pub u8, pub u8);

impl Display for Interval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let l = escape_default(self.0);
        let r = escape_default(self.1);
        if self.0 == self.1 {
            write!(f, "{l}")
        } else {
            write!(f, "[{l}, {r}]")
        }
    }
}

impl Interval {
    pub fn new(start: u8, end: u8) -> Self {
        Self(start, end)
    }
    pub fn mangle(&self) -> String {
        if self.0 == self.1 {
            format!("C{:X}X", self.0)
        } else {
            format!("R{:X}X{:X}X", self.0, self.1)
        }
    }
    // Check if two intervals overlap.
    pub fn overlaps(&self, other: &Self) -> bool {
        self.0 <= other.1 && other.0 <= self.1
    }
    // Check if two intervals are consecutive.
    pub fn is_consecutive(&self, other: &Self) -> bool {
        (self.1 != u8::MAX && self.1 + 1 == other.0)
            || (other.1 != u8::MAX && other.1 + 1 == self.0)
    }
    // Merge two intervals.
    pub fn merge(&self, other: &Self) -> Self {
        debug_assert!(self.overlaps(other) || self.is_consecutive(other));
        Self::new(self.0.min(other.0), self.1.max(other.1))
    }
    pub fn intersection(&self, other: &Self) -> Self {
        debug_assert!(self.overlaps(other));
        Self::new(self.0.max(other.0), self.1.min(other.1))
    }
    pub fn contains(&self, other: &Self) -> bool {
        self.0 <= other.0 && other.1 <= self.1
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Intervals(SmallVec<[Interval; 2]>);

impl Intervals {
    pub fn new<I>(data: I) -> Option<Self>
    where
        I: IntoIterator<Item = Interval>,
    {
        let mut data = data.into_iter();
        let mut ret = Self(smallvec![data.next()?]);
        for x in data {
            ret = ret.union(&Self(smallvec![x]));
        }
        Some(ret)
    }

    pub fn is_single_byte(&self) -> bool {
        self.0.len() == 1 && self.0[0].0 == self.0[0].1
    }
    
    pub fn representative(&self) -> u8 {
        self.0[0].0
    }

    pub fn mangle(&self) -> String {
        let mut result = String::new();
        for i in self.0.iter() {
            result.push_str(&i.mangle());
        }
        format!("S{}{}", result.len(), result)
    }

    pub fn is_full_set(&self) -> bool {
        if self.0.len() == 1 &&
            let Some(x) = self.0.first() {
            x.0 == 0 && x.1 == u8::MAX
        } else {
            false
        }
    }

    // it is okay is contains non-unicode code points; they will never be read anyway.
    pub fn complement(&self) -> Option<Self> {
        let mut current = Some(0u8);
        let mut result = SmallVec::new();
        for i in self.0.iter() {
            if let Some(current) = current && current < i.0 {
                result.push(Interval::new(current, i.0 - 1));
            }
            current = i.1.checked_add(1);
        }
        if let Some(current) = current {
            result.push(Interval::new(current, u8::MAX));
        }
        if result.is_empty() {
            None
        } else {
            Some(Self(result))
        }
    }

    pub fn contains(&self, target: u8) -> bool {
        match self.0.binary_search_by_key(&target, |x| x.0) {
            Ok(_) => true,
            Err(idx) => {
                if idx == 0 {
                    false
                } else {
                    let idx = idx - 1;
                    self.0[idx].1 >= target
                }
            }
        }
    }

    pub fn intersection(&self, other: &Self) -> Option<Self> {
        let mut result: Option<Self> = None;
        for i in self.0.iter().copied() {
            'inner: for j in other.0.iter().copied() {
                if i.overlaps(&j) {
                    let temp = i.intersection(&j);
                    result = match result {
                        None => Self::new([temp]),
                        Some(x) => Some(x.union(&Self::new([temp])?)),
                    };
                } else if j.0 > i.1 {
                    break 'inner;
                }
            }
        }
        result
    }

    pub fn union(&self, other: &Self) -> Self {
        let mut result = SmallVec::new();
        let mut i = self.0.iter().copied();
        let mut j = other.0.iter().copied();
        let mut x = i.next();
        let mut y = j.next();
        let mut current = None;
        loop {
            if current.is_none() {
                current = match (x, y) {
                    (Some(x), Some(y)) if x.0 < y.0 => Some(x),
                    (_, Some(y)) => Some(y),
                    (Some(x), _) => Some(x),
                    _ => break,
                };
            }
            current = {
                let current = unsafe { current.unwrap_unchecked() };
                if let Some(ix) = x &&
                    (current.overlaps(&ix) || current.is_consecutive(&ix)) {
                    x = i.next();
                    Some(current.merge(&ix))
                } else if let Some(iy) = y &&
                    (current.overlaps(&iy) || current.is_consecutive(&iy)) {
                    y = j.next();
                    Some(current.merge(&iy))
                } else {
                    result.push(current);
                    None
                }
            }
        }
        Self(result)
    }
}

impl Display for Intervals {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0.as_slice() {
            [] => Ok(()),
            [single] => write!(f, "{single}"),
            multiple => {
                let iter = multiple.iter().map(|i| i.to_string());
                write!(f, "({})", iter.collect::<Vec<_>>().join(" | "))
            }
        }
    }
}

impl ToTokens for Intervals {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        debug_assert!(!self.0.is_empty());
        let iter = self.0.iter().map(|Interval(start, end)| {
            if start == end {
                quote! { #start }
            } else {
                quote! { #start ..= #end }
            }
        });
        tokens.extend(quote! { #(#iter)|* });
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn basic_format() {
        let interval = super::Interval::new(0x41, 0x5A);
        assert_eq!(format!("{interval}"), "[A, Z]");
        let interval = super::Interval::new(0x41, 0x7A);
        assert_eq!(format!("{interval}"), "[A, z]");
        let interval = super::Interval::new(0x41, 0x7B);
        assert_eq!(format!("{interval}"), "[A, {]");
        // whitespace
        let interval = super::Interval::new(b'\t', b'\t');
        assert_eq!(format!("{interval}"), r"\t");
    }

    #[test]
    fn intervals_format() {
        let intervals = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9')).unwrap();
        assert_eq!(format!("{intervals}"), "([0, 9] | [A, Z] | [a, z])");
    }

    #[test]
    fn union() {
        let x = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9')).unwrap();
        assert_eq!(x.union(&x), x);
        let y = intervals!(('!', '7')).unwrap();
        assert_eq!(
            intervals!(('!', '9'), ('A', 'Z'), ('a', 'z')).unwrap(),
            x.union(&y)
        );
        let z = intervals!(('!', '7'), ('C', 'e')).unwrap();
        assert_eq!(intervals!(('!', '9'), ('A', 'z')).unwrap(), x.union(&z));
    }

    #[test]
    fn complement() {
        let x = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9')).unwrap();
        let y = intervals!((0, 47), (58, 64), (91, 96), (123, u8::MAX)).unwrap();
        assert_eq!(x.complement(), Some(y));
        let z = intervals!(('\0', '7')).unwrap();
        assert_eq!(z.complement().unwrap(), intervals!(('8', u8::MAX)).unwrap());
        assert_eq!(x.complement().unwrap().complement().unwrap(), x);
        assert_eq!(
            x.union(&x.complement().unwrap()),
            intervals!((0, u8::MAX)).unwrap()
        );
    }

    #[test]
    fn intersection() {
        let x = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9')).unwrap();
        let z = intervals!(('\0', '7')).unwrap();
        assert_eq!(x.intersection(&z), Some(intervals!(('0', '7')).unwrap()));
        assert!(x.intersection(&x.complement().unwrap()).is_none());
        assert_eq!(
            x.intersection(&intervals!((0, u8::MAX)).unwrap()).unwrap(),
            x
        );
        let a = intervals!(('E', 'c')).unwrap();
        assert_eq!(
            x.intersection(&a),
            Some(intervals!(('E', 'Z'), ('a', 'c')).unwrap())
        );
    }
}
