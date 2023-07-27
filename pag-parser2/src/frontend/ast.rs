// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;

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
    pub ty: syn::Type,
    pub rules: Vec<ParserRule>,
}

pub struct ParserRule {
    pub vars: Vec<VarBinding>,
    pub action: Option<syn::Block>,
}

pub struct VarBinding {
    pub expr: ParserExpr,
    pub name: Option<syn::Ident>,
    pub ty: Option<syn::Type>,
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
}
