// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::{HashMap, HashSet};

use crate::{
    core_syntax::BindingContext,
    core_syntax::{ParserRule, Term, TermArena, TermPtr},
    nf::Tag,
    span_errors,
    type_system::{type_check, TypeError},
    utilities::{merge_results, unreachable_branch, Symbol},
};

use super::{
    lexical::LexerDatabase,
    FrontendResult,
    SurfaceSyntaxTree::{self, *},
    WithSpan,
};

pub struct Parser<'src, 'a> {
    pub entrypoint: Symbol<'src>,
    pub arena: &'a TermArena<'src, 'a>,
    pub bindings: BindingContext<'src, 'a>,
    pub symbol_set: HashSet<&'src str>,
    pub lexer_database: LexerDatabase<'src>,
}

impl<'src, 'a> Parser<'src, 'a> {
    pub fn type_check(&self) -> Vec<TypeError<'src>> {
        let target = self.bindings.get(&self.entrypoint).unwrap();
        type_check(&self.bindings, target.term, self.entrypoint)
    }

    pub fn is_active(&self, tag: &Tag<'src>) -> bool {
        !tag.is_versioned() && self.bindings.get(&tag.symbol()).map_or(false, |x| x.active)
    }
}

pub fn construct_parser<'src, 'a>(
    arena: &'a TermArena<'src, 'a>,
    lexer_database: LexerDatabase<'src>,
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> FrontendResult<'src, Parser<'src, 'a>> {
    let symbol_set = construct_symbol_table(sst)?;
    let ParserDef { entrypoint, rules } = &sst.node else {
        unreachable_branch!("sst should be a parser definition")
    };
    let entrypoint = match symbol_set.get(entrypoint.span.as_str()) {
        Some(name) => Symbol::new(name),
        None => {
            return Err(span_errors!(
                UndefinedParserRuleReference,
                entrypoint.span,
                entrypoint.span.as_str(),
            ));
        }
    };
    let mut parser = Parser {
        entrypoint,
        arena,
        bindings: HashMap::new(),
        lexer_database,
        symbol_set,
    };
    let mut errs = Vec::new();
    for rule in rules {
        let ParserRuleDef {
            active,
            fixpoint,
            name,
            expr,
        } = &rule.node else {
            unreachable_branch!("parser should only contain rule definitions")
        };
        match construct_core_syntax_tree(&parser, expr) {
            Ok(expr) => {
                let symbol = Symbol::new(name.span.as_str());
                let node = if *fixpoint {
                    Term::Fix(symbol, expr)
                } else {
                    expr.node.clone()
                };
                parser.bindings.insert(
                    symbol,
                    ParserRule {
                        active: *active,
                        term: parser.arena.alloc(WithSpan {
                            span: rule.span,
                            node,
                        }),
                    },
                );
            }
            Err(e) => errs.extend(e),
        }
    }
    if !errs.is_empty() {
        return Err(errs);
    }
    Ok(parser)
}

fn construct_symbol_table<'src>(
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> FrontendResult<'src, HashSet<&'src str>> {
    let ParserDef { rules, .. } = &sst.node else {
        unreachable_branch!("sst should be a parser definition")
    };
    let mut symbol_table = HashMap::with_capacity(rules.len());
    for rule in rules {
        let ParserRuleDef { name, .. } = &rule.node else {
            unreachable_branch!("parser should only contain rule definitions")
        };
        if let Some(previous) = symbol_table.get(name.span.as_str()) {
            return Err(span_errors!(
                MultipleDefinition,
                name.span,
                name.span.as_str(),
                *previous,
            ));
        } else {
            symbol_table.insert(name.span.as_str(), name.span);
        }
    }
    Ok(symbol_table.keys().copied().collect())
}

fn construct_core_syntax_tree<'src, 'a>(
    context: &Parser<'src, 'a>,
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> FrontendResult<'src, TermPtr<'src, 'a>> {
    let spanned = |node: Term<'src, 'a>| {
        context.arena.alloc(WithSpan {
            span: sst.span,
            node,
        })
    };
    match &sst.node {
        ParserAlternative { lhs, rhs } => {
            let lhs = construct_core_syntax_tree(context, lhs);
            let rhs = construct_core_syntax_tree(context, rhs);
            merge_results(lhs, rhs, |l, r| &*spanned(Term::Alternative(l, r)))
        }
        ParserSequence { lhs, rhs } => {
            let lhs = construct_core_syntax_tree(context, lhs);
            let rhs = construct_core_syntax_tree(context, rhs);
            merge_results(lhs, rhs, |l, r| &*spanned(Term::Sequence(l, r)))
        }
        ParserStar { inner } => {
            let symbol = Symbol::new(sst.span.as_str());
            let inner = construct_core_syntax_tree(context, inner)?;
            // \x . (i ~ x) | epsilon
            let sequence = spanned(Term::Sequence(inner, spanned(Term::ParserRef(symbol))));
            let alternative = spanned(Term::Alternative(sequence, spanned(Term::Epsilon)));
            Ok(spanned(Term::Fix(symbol, alternative)))
        }
        ParserPlus { inner } => {
            let symbol = Symbol::new(sst.span.as_str());
            let inner = construct_core_syntax_tree(context, inner)?;
            // i ~ (\x . (i ~ x) | epsilon)
            let sequence = spanned(Term::Sequence(inner, spanned(Term::ParserRef(symbol))));
            let alternative = spanned(Term::Alternative(sequence, spanned(Term::Epsilon)));
            let fixpoint = spanned(Term::Fix(symbol, alternative));
            Ok(spanned(Term::Sequence(inner, fixpoint)))
        }
        ParserOptional { inner } => {
            let inner = construct_core_syntax_tree(context, inner)?;
            Ok(spanned(Term::Alternative(inner, spanned(Term::Epsilon))))
        }
        Bottom => Ok(spanned(Term::Bottom)),
        Empty => Ok(spanned(Term::Epsilon)),
        ParserRuleRef { name } => {
            let name = name.span.as_str();
            match context.symbol_set.get(name) {
                Some(target) => Ok(spanned(Term::ParserRef(Symbol::new(target)))),
                None => Err(span_errors!(UndefinedParserRuleReference, sst.span, name)),
            }
        }
        LexicalRuleRef { name } => {
            let name = name.span.as_str();
            match context.lexer_database.symbol_table.get(name) {
                Some(target) => Ok(spanned(Term::LexerRef(*target))),
                None => Err(span_errors!(UndefinedLexicalReference, sst.span, name)),
            }
        }
        _ => unreachable_branch!(
            "core syntax tree construction should not be called on this node: {}",
            sst.span.as_str()
        ),
    }
}
