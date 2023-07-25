// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use super::ast::*;

use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::{parenthesized, parse_quote, Token};

use std::collections::HashMap;

#[derive(PartialEq, Eq)]
enum IdentKind {
    LexerName,
    ParserName,
    Invalid,
}

fn ident_kind(ident: &syn::Ident) -> IdentKind {
    let s = ident.to_string();
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
                let ident = input.parse::<syn::Ident>()?.unraw();
                match ident.to_string().as_str() {
                    "entry" => {
                        input.parse::<Token![=]>()?;
                        entry = Some(input.parse::<syn::Ident>()?);
                    }
                    "skip" => {
                        input.parse::<Token![=]>()?;
                        skip = Some(input.parse::<LexerExpr>()?);
                    }
                    _ => return Err(syn::Error::new(ident.span(), "invalid keyword")),
                }
            } else {
                // parse lexer / parser definitions
                let ident = input.parse::<syn::Ident>()?.unraw();
                match ident_kind(&ident) {
                    IdentKind::LexerName => {
                        input.parse::<Token![=]>()?;
                        lexer_map.insert(ident, input.parse::<LexerExpr>()?);
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

        let mut rules = Vec::new();
        loop {
            rules.push(input.parse::<ParserRule>()?);
            if !input.peek(Token![|]) {
                break;
            }
            input.parse::<Token![|]>()?;
        }

        Ok(Self { ty, rules })
    }
}

impl Parse for ParserRule {
    // (ParserBinding)+ syn::Block?
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut vars = Vec::new();
        while !input.peek(syn::token::Brace) && !input.peek(Token![|]) && !input.peek(Token![;]) {
            vars.push(input.parse::<VarBinding>()?);
        }

        let mut action = None;
        if input.peek(syn::token::Brace) {
            action = Some(input.parse::<syn::Block>()?);
        }

        Ok(Self { vars, action })
    }
}

impl Parse for VarBinding {
    // ("$" syn::Ident ("<" syn::Type ">")? ":")? ParserTree
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut name = None;
        let mut ty = None;

        if input.peek(Token![$]) {
            input.parse::<Token![%]>()?;
            name = Some(input.parse::<syn::Ident>()?.unraw());

            if input.peek(Token![<]) {
                input.parse::<Token![<]>()?;
                ty = Some(input.parse::<syn::Type>()?);
                input.parse::<Token![>]>()?;
            }

            input.parse::<Token![:]>()?;
        }

        let expr = input.parse::<ParserExpr>()?;

        Ok(Self { name, ty, expr })
    }
}

impl Parse for LexerExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_lexer_expr(input, 0)
    }
}

// pratt parsing
fn parse_lexer_expr(input: ParseStream, min_bp: u32) -> syn::Result<LexerExpr> {
    let mut lhs = 'lhs: {
        if input.peek(syn::Ident) {
            let ident = input.parse::<syn::Ident>()?.unraw();
            if ident_kind(&ident) != IdentKind::LexerName {
                return Err(syn::Error::new(ident.span(), "invalid ident"));
            }
            break 'lhs LexerExpr::Ref(ident);
        }
        if input.peek(syn::LitStr) {
            let str = input.parse::<syn::LitStr>()?;
            break 'lhs LexerExpr::Str(str);
        }
        if input.peek(syn::LitChar) {
            let l = input.parse::<syn::LitChar>()?;
            input.parse::<Token![..]>()?;
            let r = input.parse::<syn::LitChar>()?;
            break 'lhs LexerExpr::Range(l, r);
        }
        if input.peek(syn::token::Paren) {
            let content;
            parenthesized!(content in input);
            break 'lhs content.parse::<LexerExpr>()?;
        }
        if input.peek(Token![!]) {
            input.parse::<Token![!]>()?;
            let r_bp = 60;
            let rhs = parse_lexer_expr(input, r_bp)?;
            break 'lhs LexerExpr::Not(Box::new(rhs));
        }
        return Err(input.error("expect lexer expression"));
    };

    loop {
        if input.peek(Token![|]) {
            let (l_bp, r_bp) = (30, 31);
            if l_bp < min_bp {
                break;
            }
            input.parse::<Token![|]>()?;
            let rhs = parse_lexer_expr(input, r_bp)?;
            lhs = LexerExpr::Alt(Box::new(lhs), Box::new(rhs));
            continue;
        }
        if input.peek(syn::Ident)
            || input.peek(syn::LitStr)
            || input.peek(syn::LitChar)
            || input.peek(syn::token::Paren)
            || input.peek(syn::token::Paren)
            || input.peek(Token![!])
        {
            let (l_bp, r_bp) = (40, 41);
            if l_bp < min_bp {
                break;
            }
            let rhs = parse_lexer_expr(input, r_bp)?;
            lhs = LexerExpr::Seq(Box::new(lhs), Box::new(rhs));
            continue;
        }
        if input.peek(Token![&]) {
            let (l_bp, r_bp) = (50, 51);
            if l_bp < min_bp {
                break;
            }
            input.parse::<Token![&]>()?;
            let rhs = parse_lexer_expr(input, r_bp)?;
            lhs = LexerExpr::And(Box::new(lhs), Box::new(rhs));
            continue;
        }
        if input.peek(Token![*]) {
            let l_bp = 70;
            if l_bp < min_bp {
                break;
            }
            input.parse::<Token![*]>()?;
            lhs = LexerExpr::Star(Box::new(lhs));
            continue;
        }
        if input.peek(Token![+]) {
            let l_bp = 80;
            if l_bp < min_bp {
                break;
            }
            input.parse::<Token![+]>()?;
            lhs = LexerExpr::Plus(Box::new(lhs));
            continue;
        }
        if input.peek(Token![?]) {
            let l_bp = 90;
            if l_bp < min_bp {
                break;
            }
            input.parse::<Token![?]>()?;
            lhs = LexerExpr::Opt(Box::new(lhs));
            continue;
        }
        break;
    }

    Ok(lhs)
}

impl Parse for ParserExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_parser_expr(input, 0)
    }
}

// pratt parsing
fn parse_parser_expr(input: ParseStream, min_bp: u32) -> syn::Result<ParserExpr> {
    let mut lhs = 'lhs: {
        if input.peek(syn::Ident) {
            let ident = input.parse::<syn::Ident>()?.unraw();
            match ident_kind(&ident) {
                IdentKind::LexerName => break 'lhs ParserExpr::LexerRef(ident),
                IdentKind::ParserName => break 'lhs ParserExpr::ParserRef(ident),
                _ => return Err(syn::Error::new(ident.span(), "invalid ident")),
            }
        }
        return Err(input.error("expect lexer expression"));
    };

    loop {
        if input.peek(syn::Ident) {
            let (l_bp, r_bp) = (40, 41);
            if l_bp < min_bp {
                break;
            }
            let rhs = parse_parser_expr(input, r_bp)?;
            lhs = ParserExpr::Seq(Box::new(lhs), Box::new(rhs));
            continue;
        }
        if input.peek(Token![*]) {
            let l_bp = 70;
            if l_bp < min_bp {
                break;
            }
            input.parse::<Token![*]>()?;
            lhs = ParserExpr::Star(Box::new(lhs));
            continue;
        }
        if input.peek(Token![+]) {
            let l_bp = 80;
            if l_bp < min_bp {
                break;
            }
            input.parse::<Token![+]>()?;
            lhs = ParserExpr::Plus(Box::new(lhs));
            continue;
        }
        if input.peek(Token![?]) {
            let l_bp = 90;
            if l_bp < min_bp {
                break;
            }
            input.parse::<Token![?]>()?;
            lhs = ParserExpr::Opt(Box::new(lhs));
            continue;
        }
        break;
    }

    Ok(lhs)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lexer_expr() {
        syn::parse_str::<LexerExpr>(r#"("abc" 'a'..'z') r#A | B & C | D* E+ F? !G"#).unwrap();
    }

    #[test]
    fn test_parser_expr() {
        syn::parse_str::<ParserExpr>(r#"A? b c* D+ F?"#).unwrap();
    }
}
