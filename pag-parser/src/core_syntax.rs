// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;
use std::fmt::Display;

use typed_arena::Arena;

use crate::frontend::WithSpan;
use crate::utilities::Symbol;

#[derive(Debug, Clone)]
pub enum Term<'src, 'arena> {
    Epsilon,
    Sequence(TermPtr<'src, 'arena>, TermPtr<'src, 'arena>),
    LexerRef(Symbol<'src>),
    Bottom,
    Alternative(TermPtr<'src, 'arena>, TermPtr<'src, 'arena>),
    Fix(Symbol<'src>, TermPtr<'src, 'arena>),
    ParserRef(Symbol<'src>),
}

pub type TermPtr<'src, 'arena> = &'arena WithSpan<'src, Term<'src, 'arena>>;
pub type TermArena<'src, 'arena> = Arena<WithSpan<'src, Term<'src, 'arena>>>;

pub struct ParserRule<'src, 'arena> {
    pub active: bool,
    pub term: TermPtr<'src, 'arena>,
}

pub type BindingContext<'src, 'arena> = HashMap<Symbol<'src>, ParserRule<'src, 'arena>>;

impl Display for Term<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Epsilon => write!(f, "ε"),
            Term::Sequence(x, y) => write!(f, "({x} ~ {y})"),
            Term::LexerRef(x) => write!(f, "{x}"),
            Term::Bottom => write!(f, "⊥"),
            Term::Alternative(x, y) => write!(f, "({x} | {y})"),
            Term::Fix(x, y) => write!(f, "(μ {x} . {y})"),
            Term::ParserRef(x) => write!(f, "{x}"),
        }
    }
}
