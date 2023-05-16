use std::{fmt::Display, todo};

use smallvec::SmallVec;
use typed_arena::Arena;

use crate::{utilities::Symbol, core_syntax::BindingContext};

// thinking a while...

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tag<'src> {
    symbol: Symbol<'src>,
    version: usize,
}

impl <'src> Tag<'src> {
    fn new(symbol: Symbol<'src>, version: usize) -> Self {
        Self { symbol, version }
    }
    fn next(&self) -> Self {
        Self::new(self.symbol, self.version + 1)
    }
}

pub enum NormalForm<'src, 'a> {
    Empty(Tag<'src>),
    Sequence { 
        tag: Tag<'src>, 
        terminal: Symbol<'src>, 
        nonterminals: SmallVec<[&'a NormalForm<'src, 'a>; 4]> 
    },
}

impl<'src, 'a> NormalForm<'src, 'a>  {
    pub fn tag(&self) -> Tag<'src> {
        match self {
            NormalForm::Empty(tag) => *tag,
            NormalForm::Sequence { tag, .. } => *tag,
        }
    }
}

impl<'src> Display for Tag<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.symbol, self.version)
    }
}

impl<'src, 'a> Display for NormalForm<'src, 'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NormalForm::Empty(tag) => {
                write!(f, "{} -> ε", tag)
            }
            NormalForm::Sequence { tag, terminal, nonterminals } => {
                write!(f, "{} -> {terminal}", tag, )?;
                for i in nonterminals {
                    write!(f, " {}", i.tag())?;
                }
                Ok(())
            }
        }
    }
}

// need a context for fixpoint
pub fn normalize<'src, 'p, 'nf>(
    target: Symbol<'src>,
    bindings: &'p BindingContext<'src, 'p>,
    arena: &'nf Arena<NormalForm<'src, 'nf>>,
) -> SmallVec<[&'nf NormalForm<'src, 'nf>; 4]> {
    match &bindings.get(&target).unwrap().term.node {
        crate::core_syntax::Term::Epsilon => {
            [ arena.alloc(NormalForm::Empty(Tag::new(target, 0))) as &'nf _ ].into_iter().collect()
        },
        crate::core_syntax::Term::Sequence(_, _) => todo!(),
        crate::core_syntax::Term::LexerRef(lexer) => {
            [ arena.alloc(NormalForm::Sequence { 
                tag: Tag::new(target, 0), 
                terminal: *lexer, 
                nonterminals: SmallVec::new() 
            }) as &'nf _ ].into_iter().collect()
        },
        crate::core_syntax::Term::Bottom => {
            [].into_iter().collect()
        },
        crate::core_syntax::Term::Alternative(_, _) => todo!(),
        crate::core_syntax::Term::Fix(_, _) => todo!(),
        crate::core_syntax::Term::ParserRef(_) => todo!(),
    }
}