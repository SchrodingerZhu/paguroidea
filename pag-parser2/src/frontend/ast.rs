// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;
use std::rc::Rc;

pub struct Ast {
    pub entry: syn::Ident,
    pub skip: Option<LexerExpr>,
    pub lexer_map: HashMap<syn::Ident, LexerDef>,
    pub parser_map: HashMap<syn::Ident, ParserDef>,
}

#[derive(Clone)]
#[repr(transparent)]
pub struct CustomizedBlock(pub Rc<syn::Block>);

impl PartialEq for CustomizedBlock {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for CustomizedBlock {}

impl PartialOrd for CustomizedBlock {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Rc::as_ptr(&self.0).partial_cmp(&Rc::as_ptr(&other.0))
    }
}

impl Ord for CustomizedBlock {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Rc::as_ptr(&self.0).cmp(&Rc::as_ptr(&other.0))
    }
}

impl std::hash::Hash for CustomizedBlock {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}

pub struct LexerDef {
    pub idx: u32,
    pub expr: LexerExpr,
}

pub struct ParserDef {
    pub ty: syn::Type,
    pub rules: Vec<ParserRule>,
}

pub struct ParserRule {
    pub vars: Vec<VarBinding>,
    pub action: Option<CustomizedBlock>,
}

pub struct VarBinding {
    pub expr: ParserExpr,
    pub name: Option<syn::Ident>,
}

// TODO: how to express "bottom" & "any"?
pub enum LexerExpr {
    Alt(Box<Self>, Box<Self>),
    Seq(Box<Self>, Box<Self>),
    And(Box<Self>, Box<Self>),
    Star(Box<Self>),
    Plus(Box<Self>),
    Opt(Box<Self>),
    Not(Box<Self>),
    Ref(syn::Ident),
    Str(syn::LitStr),
    Range(syn::LitChar, syn::LitChar),
}

// TODO: how to express "select" & "ignore"?
pub enum ParserExpr {
    Seq(Box<Self>, Box<Self>),
    Star(Box<Self>),
    Plus(Box<Self>),
    Opt(Box<Self>),
    LexerRef(syn::Ident),
    ParserRef(syn::Ident),
    Ignore(Box<Self>),
    Hinted(Box<Self>, syn::Type),
}

pub struct RightDeepIterator<'a> {
    seq: Option<&'a ParserExpr>,
}

impl<'a> From<&'a ParserExpr> for RightDeepIterator<'a> {
    fn from(expr: &'a ParserExpr) -> Self {
        Self { seq: Some(expr) }
    }
}

impl<'a> Iterator for RightDeepIterator<'a> {
    type Item = &'a ParserExpr;

    fn next(&mut self) -> Option<Self::Item> {
        match self.seq {
            Some(ParserExpr::Seq(a, b)) => {
                self.seq = Some(b);
                Some(a)
            }
            Some(_) => self.seq.take(),
            None => None,
        }
    }
}
