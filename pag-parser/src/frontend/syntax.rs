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
    type_system::{infer_fixpoints, type_check, TypeError},
    utilities::{merge_results, unreachable_branch, Symbol},
};

use super::{
    lexical::LexerDatabase,
    FrontendError::*,
    FrontendResult,
    SurfaceSyntaxTree::{self, *},
    WithSpan,
};

pub struct Parser<'src, 'arena> {
    pub entrypoint: Symbol<'src>,
    pub arena: &'arena TermArena<'src, 'arena>,
    pub bindings: BindingContext<'src, 'arena>,
    pub symbol_set: HashSet<&'src str>,
    pub lexer_database: LexerDatabase<'src>,
}

impl<'src, 'arena> Parser<'src, 'arena> {
    pub fn infer_fixpoints(&mut self) {
        infer_fixpoints(self.entrypoint, self.arena, &mut self.bindings);
    }

    pub fn type_check(&self) -> Vec<TypeError<'src>> {
        let target = &self.bindings[&self.entrypoint];
        type_check(&self.bindings, target.term, self.entrypoint)
    }

    pub fn is_active(&self, tag: &Tag<'src>) -> bool {
        tag.is_original() && self.bindings.get(&tag.symbol()).map_or(false, |x| x.active)
    }
}

pub fn construct_parser<'src, 'arena>(
    arena: &'arena TermArena<'src, 'arena>,
    lexer_database: LexerDatabase<'src>,
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> FrontendResult<'src, Parser<'src, 'arena>> {
    let ParserDef { entrypoint, rules } = &sst.node else {
        unreachable_branch!("sst should be a parser definition")
    };
    let symbol_set = construct_symbol_set(sst)?;
    let entrypoint = match symbol_set.get(entrypoint.span.as_str()) {
        Some(name) => Symbol::new(name),
        None => {
            return Err(vec![UndefinedParserRuleReference(entrypoint.span)]);
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
        let ParserRuleDef { active, name, expr } = &rule.node else {
            unreachable_branch!("parser should only contain rule definitions")
        };
        match construct_core_syntax_tree(&parser, expr) {
            Ok(term) => {
                let symbol = Symbol::new(name.span.as_str());
                parser.bindings.insert(
                    symbol,
                    ParserRule {
                        active: *active,
                        term,
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

fn construct_symbol_set<'src>(
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
            return Err(vec![MultipleDefinition(*previous, name.span)]);
        } else {
            symbol_table.insert(name.span.as_str(), name.span);
        }
    }
    Ok(symbol_table.keys().copied().collect())
}

fn construct_core_syntax_tree<'src, 'arena>(
    context: &Parser<'src, 'arena>,
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> FrontendResult<'src, TermPtr<'src, 'arena>> {
    let spanned = |node: Term<'src, 'arena>| {
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
            match context.symbol_set.get(name.span.as_str()) {
                // Symbol::hash depends on the address so use the original &str
                Some(target) => Ok(spanned(Term::ParserRef(Symbol::new(target)))),
                None => Err(vec![UndefinedParserRuleReference(name.span)]),
            }
        }
        LexicalRuleRef { name } => {
            match context.lexer_database.symbol_set.get(name.span.as_str()) {
                // Symbol::hash depends on the address so use the original &str
                Some(target) => Ok(spanned(Term::LexerRef(Symbol::new(target)))),
                None => Err(vec![UndefinedLexicalRuleReference(name.span)]),
            }
        }
        _ => unreachable_branch!("called with unsupported node: {}", sst.span.as_str()),
    }
}
