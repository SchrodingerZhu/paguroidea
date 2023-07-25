// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

#[derive(Debug, Clone, Copy, PartialOrd, Ord)]
pub struct Symbol<'a>(&'a str);

impl<'a> Symbol<'a> {
    pub fn new(data: &'a str) -> Self {
        Self(data)
    }

    pub fn name(&self) -> &'a str {
        self.0
    }
}

impl<'a> std::hash::Hash for Symbol<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
        self.0.len().hash(state);
    }
}

impl<'a, 'b> PartialEq<Symbol<'b>> for Symbol<'a> {
    fn eq(&self, other: &Symbol<'b>) -> bool {
        self.0.as_ptr() == other.0.as_ptr() && self.0.len() == other.0.len()
    }
}

impl<'a> Eq for Symbol<'a> {}

fn is_ascii_ident_body(x: &u8) -> bool {
    x.is_ascii_alphanumeric() || *x == b'_'
}

fn is_ascii_ident_head(x: &u8) -> bool {
    x.is_ascii_alphabetic() || *x == b'_'
}

fn is_ascii_ident(s: &str) -> bool {
    let [x, xs @ ..] = s.as_bytes() else {
        return false;
    };
    is_ascii_ident_head(x) && xs.iter().all(is_ascii_ident_body)
}

impl<'a> std::fmt::Display for Symbol<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if is_ascii_ident(self.0) {
            write!(f, "{}", self.0)
        } else {
            write!(f, "s{:x}_{}", self.0.as_ptr() as usize, self.0.len())
        }
    }
}

pub fn merge_results<T, E, U>(
    a: Result<T, Vec<E>>,
    b: Result<T, Vec<E>>,
    f: impl FnOnce(T, T) -> U,
) -> Result<U, Vec<E>> {
    match (a, b) {
        (Ok(a), Ok(b)) => Ok(f(a, b)),
        (Ok(_), Err(b)) => Err(b),
        (Err(a), Ok(_)) => Err(a),
        (Err(mut a), Err(b)) => {
            a.extend(b);
            Err(a)
        }
    }
}

macro_rules! unreachable_branch {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) {
            unreachable!($($arg)*)
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    };
}

pub(crate) use unreachable_branch;
