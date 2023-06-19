// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use proc_macro2::{Literal, TokenStream};
use quote::{quote, ToTokens};
use smallvec::{smallvec, SmallVec};
use std::fmt::{Display, Formatter};

#[macro_export]
macro_rules! interval {
    ($start:expr, $end:expr) => {
        $crate::intervals::Interval($start as u8, $end as u8)
    };
}

#[macro_export]
macro_rules! intervals {
    ($(($start:expr, $end:expr)),* $(,)?) => {
        $crate::intervals::Intervals::new(
            [$($crate::interval!($start, $end)),+]
        )
    };
}

// A closed interval of u8s.
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
pub struct Interval(pub u8, pub u8);

impl Display for Interval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let start = self.0.escape_ascii();
        let end = self.1.escape_ascii();
        if self.0 == self.1 {
            write!(f, "{start}")
        } else {
            write!(f, "[{start}, {end}]")
        }
    }
}

impl Interval {
    // Check if two intervals overlap.
    pub fn overlaps(&self, other: &Self) -> bool {
        self.0 <= other.1 && other.0 <= self.1
    }

    pub fn intersection(&self, other: &Self) -> Option<Self> {
        if self.overlaps(other) {
            Some(Self(self.0.max(other.0), self.1.min(other.1)))
        } else {
            None
        }
    }

    pub fn contains(&self, other: &Self) -> bool {
        self.0 <= other.0 && other.1 <= self.1
    }
}

// Invariants:
// - Ordered
// - Non-consecutive
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Intervals(SmallVec<[Interval; 8]>);

impl Intervals {
    pub fn new<I>(data: I) -> Self
    where
        I: IntoIterator<Item = Interval>,
    {
        data.into_iter()
            .map(|x| Self(smallvec![x]))
            .reduce(|acc, x| acc.union(&x))
            .unwrap_or_default()
    }

    pub fn empty_set() -> Self {
        Self(smallvec![])
    }

    pub fn full_set() -> Self {
        Self(smallvec![Interval(u8::MIN, u8::MAX)])
    }

    pub fn iter(&self) -> impl Iterator<Item = &Interval> {
        self.0.iter()
    }

    pub fn is_single_byte(&self) -> bool {
        self.0.len() == 1 && self.0[0].0 == self.0[0].1
    }

    pub fn representative(&self) -> u8 {
        self.0[0].0
    }

    pub fn is_empty_set(&self) -> bool {
        self.0.is_empty()
    }

    pub fn is_full_set(&self) -> bool {
        self.0.first() == Some(&Interval(u8::MIN, u8::MAX))
    }

    // it is okay it contains non-unicode code points; they will never be read anyway.
    pub fn complement(&self) -> Self {
        let mut current = 0;
        let mut result = SmallVec::new();
        for &Interval(l, r) in self.0.iter() {
            if current < l {
                result.push(Interval(current, l - 1));
            }
            if r == u8::MAX {
                return Self(result);
            }
            current = r + 1;
        }
        result.push(Interval(current, u8::MAX));
        Self(result)
    }

    pub fn contains(&self, target: u8) -> bool {
        match self.0.binary_search_by_key(&target, |x| x.0) {
            Ok(_) => true,
            Err(0) => false,
            Err(idx) => self.0[idx - 1].1 >= target,
        }
    }

    pub fn intersection(&self, other: &Self) -> Self {
        let mut result = SmallVec::new();
        for x in self.0.iter() {
            for y in other.0.iter() {
                if let Some(interval) = x.intersection(y) {
                    result.push(interval);
                } else if y.0 > x.1 {
                    break;
                }
            }
        }
        result.sort_unstable_by_key(|&Interval(l, _)| l);
        Self(result)
    }

    pub fn union(&self, other: &Self) -> Self {
        let mut result = SmallVec::new();
        let mut i = self.0.iter().copied().peekable();
        let mut j = other.0.iter().copied().peekable();
        loop {
            let mut current = match (i.peek(), j.peek()) {
                (Some(&x), Some(&y)) if x.0 < y.0 => i.next().unwrap(),
                (_, Some(_)) => j.next().unwrap(),
                (Some(_), _) => i.next().unwrap(),
                _ => break,
            };
            loop {
                match (i.peek(), j.peek()) {
                    (Some(x), _) if current.1.wrapping_add(1) >= x.0 => {
                        current.1 = current.1.max(i.next().unwrap().1);
                    }
                    (_, Some(y)) if current.1.wrapping_add(1) >= y.0 => {
                        current.1 = current.1.max(j.next().unwrap().1);
                    }
                    _ => break,
                }
            }
            result.push(current);
        }
        Self(result)
    }
}

impl Display for Intervals {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0.as_slice() {
            [] => write!(f, "⊥"),
            [single] => write!(f, "{single}"),
            multiple => {
                let iter = multiple.iter().map(|i| i.to_string());
                write!(f, "({})", iter.collect::<Vec<_>>().join(" | "))
            }
        }
    }
}

pub fn byte_char(c: u8) -> Literal {
    format!("b'{}'", c.escape_ascii()).parse().unwrap()
}

impl ToTokens for Intervals {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        debug_assert!(!self.0.is_empty());
        let iter = self.0.iter().map(|Interval(start, end)| {
            let start_lit = byte_char(*start);
            let end_lit = byte_char(*end);
            if start == end {
                quote! { #start_lit }
            } else {
                quote! { #start_lit ..= #end_lit }
            }
        });
        tokens.extend(quote! { #(#iter)|* });
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn basic_format() {
        let interval = interval!(0x41, 0x5A);
        assert_eq!(format!("{interval}"), "[A, Z]");
        let interval = interval!(0x41, 0x7A);
        assert_eq!(format!("{interval}"), "[A, z]");
        let interval = interval!(0x41, 0x7B);
        assert_eq!(format!("{interval}"), "[A, {]");
        // whitespace
        let interval = interval!(b'\t', b'\t');
        assert_eq!(format!("{interval}"), r"\t");
    }

    #[test]
    fn intervals_format() {
        let intervals = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9'));
        assert_eq!(format!("{intervals}"), "([0, 9] | [A, Z] | [a, z])");
    }

    #[test]
    fn union() {
        let x = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9'));
        assert_eq!(x.union(&x), x);
        let y = intervals!(('!', '7'));
        assert_eq!(x.union(&y), intervals!(('!', '9'), ('A', 'Z'), ('a', 'z')));
        let z = intervals!(('!', '7'), ('C', 'e'));
        assert_eq!(x.union(&z), intervals!(('!', '9'), ('A', 'z')));
    }

    #[test]
    fn complement() {
        let x = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9'));
        let y = intervals!((0, 47), (58, 64), (91, 96), (123, u8::MAX));
        assert_eq!(x.complement(), y);
        let z = intervals!(('\0', '7'));
        assert_eq!(z.complement(), intervals!(('8', u8::MAX)));
        assert_eq!(x.complement().complement(), x);
        assert_eq!(x.union(&x.complement()), intervals!((0, u8::MAX)));
    }

    #[test]
    fn intersection() {
        let x = intervals!(('a', 'z'), ('A', 'Z'), ('0', '9'));
        let z = intervals!(('\0', '7'));
        assert_eq!(x.intersection(&z), intervals!(('0', '7')));
        assert!(x.intersection(&x.complement()).is_empty_set());
        assert_eq!(x.intersection(&intervals!((0, u8::MAX))), x);
        let a = intervals!(('E', 'c'));
        assert_eq!(x.intersection(&a), intervals!(('E', 'Z'), ('a', 'c')));
    }
}
