// Copyright (c) 2023 Paguroidea Developpers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{collections::HashMap, fmt::Display};

use pest::Span;

use crate::{frontend::WithSpan, utilities::Symbol};

#[derive(Debug, Clone)]
pub enum Term<'src, 'a> {
    Epsilon,
    Sequence(&'a WithSpan<'src, Self>, &'a WithSpan<'src, Self>),
    LexerRef(Symbol<'src>),
    Bottom,
    Alternative(&'a WithSpan<'src, Self>, &'a WithSpan<'src, Self>),
    // Star(&'a WithSpan<'src, Self>),
    Fix(Symbol<'src>, &'a WithSpan<'src, Self>),
    ParserRef(Symbol<'src>),
}

pub type TermPtr<'src, 'a> = &'a WithSpan<'src, Term<'src, 'a>>;
pub type TermArena<'src, 'a> = typed_arena::Arena<WithSpan<'src, Term<'src, 'a>>>;

pub fn allocate_term<'src, 'a>(
    arena: &'a TermArena<'src, 'a>,
    node: Term<'src, 'a>,
    span: Span<'src>,
) -> TermPtr<'src, 'a> {
    arena.alloc(WithSpan { span, node })
}

pub struct ParserRule<'src, 'a> {
    pub active: bool,
    pub term: TermPtr<'src, 'a>,
}

pub type BindingContext<'src, 'a> = HashMap<Symbol<'src>, ParserRule<'src, 'a>>;

impl<'src, 'a> Display for Term<'src, 'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Epsilon => {
                write!(f, "ε")
            }
            Term::Sequence(x, y) => {
                write!(f, "({x} ~ {y})",)
            }
            Term::LexerRef(x) => {
                write!(f, "{x}")
            }
            Term::Bottom => {
                write!(f, "⊥")
            }
            Term::Alternative(x, y) => {
                write!(f, "({x} | {y})")
            }
            Term::Fix(x, y) => {
                write!(f, "(μ {x} . {y})",)
            }
            Term::ParserRef(x) => {
                write!(f, "{x}")
            }
        }
    }
}
