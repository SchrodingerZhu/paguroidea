// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use super::ast::*;

use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parse_quote, Token};

use std::collections::HashMap;

enum IdentKind {
    LexerName,
    ParserName,
    Invalid,
}

fn ident_kind(ident: &syn::Ident) -> IdentKind {
    let s = ident.to_string(); // TODO: should we add a `.unraw()` ?
    if s.chars().all(|c| matches!(c, 'A'..='Z' | '0'..='9' | '_')) {
        return IdentKind::LexerName;
    }
    if s.chars().all(|c| matches!(c, 'a'..='z' | '0'..='9' | '_')) {
        return IdentKind::ParserName;
    }
    IdentKind::Invalid
}

impl Parse for Ast {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut entry = None;
        let mut skip = None;
        let mut lexer_map = HashMap::new();
        let mut parser_map = HashMap::new();

        while !input.is_empty() {
            if input.peek(Token![%]) {
                // parse keyword
                input.parse::<Token![%]>()?;
                let ident = input.parse::<syn::Ident>()?;
                match ident.to_string().as_str() {
                    "entry" => {
                        input.parse::<Token![=]>()?;
                        entry = Some(input.parse::<syn::Ident>()?);
                    }
                    "skip" => {
                        input.parse::<Token![=]>()?;
                        skip = Some(input.parse::<LexerTree>()?);
                    }
                    _ => return Err(syn::Error::new(ident.span(), "invalid keyword")),
                }
            } else {
                // parse lexer / parser definitions
                let ident = input.parse::<syn::Ident>()?;
                match ident_kind(&ident) {
                    IdentKind::LexerName => {
                        input.parse::<Token![=]>()?;
                        lexer_map.insert(ident, input.parse::<LexerTree>()?);
                    }
                    IdentKind::ParserName => {
                        parser_map.insert(ident, input.parse::<ParserDef>()?);
                    }
                    _ => return Err(syn::Error::new(ident.span(), "invalid ident")),
                }
            }
            input.parse::<Token![;]>()?;
        }

        Ok(Self {
            entry: entry.ok_or_else(|| input.error("missing %entry"))?,
            skip,
            lexer_map,
            parser_map,
        })
    }
}

impl Parse for ParserDef {
    // (":" syn::Type)? = (ParserRule)|+
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty = match input.parse::<Token![:]>() {
            Ok(_) => input.parse::<syn::Type>()?,
            Err(_) => parse_quote!(&'src str),
        };

        input.parse::<Token![=]>()?;

        // let mut rules = Vec::new();
        // loop {
        //     rules.push(input.parse::<ParserRule>()?);
        //     if !input.peek(Token![|]) {
        //         break;
        //     }
        //     input.parse::<Token![|]>();
        // }

        // TODO: check whether this is in-place
        let rules = Punctuated::<ParserRule, Token![|]>::parse_separated_nonempty(input)?
            .into_iter()
            .collect();

        Ok(Self { ty, rules })
    }
}

impl Parse for ParserRule {
    // (ParserBinding)+ syn::Block?
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut bindings = Vec::new();
        while !input.peek(syn::token::Brace) && !input.peek(Token![|]) && !input.peek(Token![;]) {
            bindings.push(input.parse::<ParserBinding>()?);
        }

        let mut action = None;
        if input.peek(syn::token::Brace) {
            action = Some(input.parse::<syn::Block>()?);
        }

        Ok(Self { bindings, action })
    }
}

impl Parse for ParserBinding {
    // ("$" syn::Ident ("<" syn::Type ">")? ":")? ParserTree
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut name = None;
        let mut ty = None;

        if input.peek(Token![$]) {
            input.parse::<Token![%]>()?;
            name = Some(input.parse::<syn::Ident>()?);

            if input.peek(Token![<]) {
                input.parse::<Token![<]>()?;
                ty = Some(input.parse::<syn::Type>()?);
                input.parse::<Token![>]>()?;
            }

            input.parse::<Token![:]>()?;
        }

        let tree = input.parse::<ParserTree>()?;

        Ok(Self { name, ty, tree })
    }
}

impl Parse for LexerTree {
    // pratt parsing
    fn parse(input: ParseStream) -> syn::Result<Self> {
        todo!()
    }
}

impl Parse for ParserTree {
    // pratt parsing
    fn parse(input: ParseStream) -> syn::Result<Self> {
        todo!()
    }
}
