// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct CodeBlock(pub Rc<syn::Block>);

impl PartialEq for CodeBlock {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for CodeBlock {}

impl PartialOrd for CodeBlock {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Rc::as_ptr(&self.0).partial_cmp(&Rc::as_ptr(&other.0))
    }
}

impl Ord for CodeBlock {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Rc::as_ptr(&self.0).cmp(&Rc::as_ptr(&other.0))
    }
}

impl std::hash::Hash for CodeBlock {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state)
    }
}

pub struct Ast {
    pub entry: syn::Ident,
    pub skip: Option<LexerExpr>,
    pub lexer_map: HashMap<syn::Ident, LexerDef>,
    pub parser_map: HashMap<syn::Ident, ParserDef>,
}

pub struct LexerDef {
    pub idx: u32,
    pub expr: LexerExpr,
}

pub struct ParserDef {
    pub ty: Rc<syn::Type>,
    pub rules: Vec<ParserRule>,
}

pub struct ParserRule {
    pub vars: Vec<VarBinding>, // len > 0
    pub action: Option<CodeBlock>,
}

pub struct VarBinding {
    pub expr: ParserExpr,
    pub name: Option<syn::Ident>,
    pub ty: Option<Rc<syn::Type>>,
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
    Seq(Vec<Self>), // len > 1
    Star(Box<Self>),
    Plus(Box<Self>),
    Opt(Box<Self>),
    LexerRef(syn::Ident),
    ParserRef(syn::Ident),
    Ignore(Box<Self>),
}
