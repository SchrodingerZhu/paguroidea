// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{collections::HashMap, rc::Rc};

use pag_lexer::{normalization::normalize, regex_tree::RegexTree};
use pest::Span;

use crate::frontend::unicode;
use crate::span_errors;
use crate::utilities::{merge_results, unreachable_branch, Symbol};

use super::{
    Error, FrontendErrors, FrontendResult,
    SurfaceSyntaxTree::{self, *},
    WithSpan,
};

type SpanRegexTree<'a> = WithSpan<'a, Rc<RegexTree>>;

pub struct LexerDatabase<'a> {
    pub symbol_table: HashMap<&'a str, Span<'a>>,
    pub entries: HashMap<Symbol<'a>, SpanRegexTree<'a>>,
    pub skip: Option<SpanRegexTree<'a>>,
}

impl<'a> LexerDatabase<'a> {
    pub fn new(sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>) -> FrontendResult<'a, Self> {
        TranslationContext::create_database(sst)
    }

    pub fn nullability_check(&self) -> FrontendErrors<'a> {
        let mut errors = Vec::new();
        for (sym, rule) in &self.entries {
            if rule.node.is_nullable() {
                errors.push(WithSpan {
                    span: rule.span,
                    node: Error::NullableToken(sym.name()),
                });
            }
        }
        if let Some(skip) = &self.skip {
            if skip.node.is_nullable() {
                errors.push(WithSpan {
                    span: skip.span,
                    node: Error::NullableToken("<skip>"),
                });
            }
        }
        errors
    }
}

struct TranslationContext<'a> {
    definitions: HashMap<&'a str, SpanRegexTree<'a>>,
    database: LexerDatabase<'a>,
}

fn construct_regex_tree<'a, F>(
    sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
    reference_handler: &F,
) -> FrontendResult<'a, Rc<RegexTree>>
where
    F: Fn(WithSpan<'a, ()>) -> FrontendResult<'a, Rc<RegexTree>>,
{
    match &sst.node {
        LexicalAlternative { lhs, rhs } => {
            let lhs = construct_regex_tree(lhs, reference_handler);
            let rhs = construct_regex_tree(rhs, reference_handler);
            merge_results(lhs, rhs, |l, r| Rc::new(RegexTree::Union(l, r)))
        }
        LexicalSequence { lhs, rhs } => {
            let lhs = construct_regex_tree(lhs, reference_handler);
            let rhs = construct_regex_tree(rhs, reference_handler);
            merge_results(lhs, rhs, |l, r| Rc::new(RegexTree::Concat(l, r)))
        }
        LexicalAnd { lhs, rhs } => {
            let lhs = construct_regex_tree(lhs, reference_handler);
            let rhs = construct_regex_tree(rhs, reference_handler);
            merge_results(lhs, rhs, |l, r| Rc::new(RegexTree::Intersection(l, r)))
        }
        LexicalStar { inner } => {
            let inner = construct_regex_tree(inner, reference_handler)?;
            Ok(Rc::new(RegexTree::KleeneClosure(inner)))
        }
        LexicalPlus { inner } => {
            let inner = construct_regex_tree(inner, reference_handler)?;
            Ok(Rc::new(RegexTree::Concat(
                inner.clone(),
                Rc::new(RegexTree::KleeneClosure(inner)),
            )))
        }
        LexicalOptional { inner } => {
            let inner = construct_regex_tree(inner, reference_handler)?;
            Ok(Rc::new(RegexTree::Union(
                inner,
                Rc::new(RegexTree::Epsilon),
            )))
        }
        LexicalNot { inner } => {
            let inner = construct_regex_tree(inner, reference_handler)?;
            Ok(Rc::new(RegexTree::Complement(inner)))
        }
        RangeLit { start, end } => Ok(unicode::encode_range(*start, *end)),
        StringLit(x) => {
            if x.is_empty() {
                Ok(Rc::new(RegexTree::Epsilon))
            } else {
                let mut iter = x.bytes();
                let fst = Rc::new(RegexTree::single(unsafe { iter.next().unwrap_unchecked() }));
                Ok(iter.fold(fst, |acc, c| {
                    Rc::new(RegexTree::Concat(acc, Rc::new(RegexTree::single(c))))
                }))
            }
        }
        Bottom => Ok(Rc::new(RegexTree::Bottom)),
        Empty => Ok(Rc::new(RegexTree::Epsilon)),
        CharLit { value } => Ok(unicode::encode_char(value.node)),
        LexicalRuleRef { name } => reference_handler(name.clone()),
        _ => unreachable_branch!("called with unsupported node: {}", sst.span.as_str()),
    }
}

fn construct_definition<'a>(
    sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
) -> FrontendResult<'a, (&'a str, SpanRegexTree<'a>)> {
    fn reference_handler(name: WithSpan<()>) -> FrontendResult<Rc<RegexTree>> {
        Err(span_errors!(
            InvalidLexicalReference,
            name.span,
            name.span.as_str(),
        ))
    }
    let LexicalDefinition { name, expr } = &sst.node else {
        unreachable_branch!("sst should be a lexical definition")
    };
    let name = name.span.as_str();
    let tree = construct_regex_tree(expr, &reference_handler)?;
    Ok((
        name,
        WithSpan {
            span: expr.span,
            node: normalize(tree),
        },
    ))
}

fn construct_lexical_rule<'a>(
    definitions: &HashMap<&'a str, SpanRegexTree<'a>>,
    sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
) -> FrontendResult<'a, (Symbol<'a>, SpanRegexTree<'a>)> {
    let reference_handler = |name: WithSpan<'a, ()>| match definitions.get(name.span.as_str()) {
        Some(x) => Ok(x.node.clone()),
        _ => Err(span_errors!(
            UndefinedLexicalReference,
            name.span,
            name.span.as_str(),
        )),
    };
    let LexicalTokenDef { name, expr, .. } = &sst.node else {
        unreachable_branch!("sst should be a lexical token")
    };
    let name = name.span.as_str();
    let tree = construct_regex_tree(expr, &reference_handler)?;
    Ok((
        Symbol::new(name),
        WithSpan {
            span: expr.span,
            node: normalize(tree),
        },
    ))
}

fn construct_skip_rule<'a>(
    definitions: &HashMap<&'a str, SpanRegexTree<'a>>,
    sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
) -> FrontendResult<'a, SpanRegexTree<'a>> {
    let reference_handler = |name: WithSpan<'a, ()>| match definitions.get(name.span.as_str()) {
        Some(x) => Ok(x.node.clone()),
        _ => Err(span_errors!(
            UndefinedLexicalReference,
            name.span,
            name.span.as_str(),
        )),
    };
    let LexicalSkipDef { expr, .. } = &sst.node else {
        unreachable_branch!("sst should be a lexical token")
    };
    let tree = construct_regex_tree(expr, &reference_handler)?;
    Ok(WithSpan {
        span: expr.span,
        node: normalize(tree),
    })
}

impl<'a> TranslationContext<'a> {
    fn new() -> Self {
        TranslationContext {
            definitions: HashMap::new(),
            database: LexerDatabase {
                entries: HashMap::new(),
                symbol_table: HashMap::new(),
                skip: None,
            },
        }
    }

    fn populate_definitions(
        &mut self,
        sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
    ) -> FrontendResult<'a, ()> {
        let LexerDef { rules } = &sst.node else {
            unreachable_branch!("sst should be a lexical definition")
        };
        let mut errs = Vec::new();
        for i in rules
            .iter()
            .filter(|x| matches!(x.node, LexicalDefinition { .. }))
            .map(construct_definition)
        {
            match i {
                Ok((symbol, regex)) => {
                    let current_span = regex.span;
                    match self.definitions.insert(symbol, regex) {
                        None => (),
                        Some(x) => {
                            errs.push(WithSpan {
                                span: current_span,
                                node: Error::MultipleDefinition(symbol, x.span),
                            });
                        }
                    }
                }
                Err(e) => errs.extend(e),
            }
        }
        if !errs.is_empty() {
            return Err(errs);
        }
        Ok(())
    }

    fn create_database(
        sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
    ) -> FrontendResult<'a, LexerDatabase<'a>> {
        let LexerDef { rules } = &sst.node else {
            unreachable_branch!("sst should be a lexical definition")
        };
        let mut ctx = Self::new();
        let mut errs = match ctx.populate_definitions(sst) {
            Ok(_) => Vec::new(),
            Err(errs) => errs,
        };
        for rule in rules {
            match &rule.node {
                LexicalSkipDef { .. } => {
                    if ctx.database.skip.is_some() {
                        errs.push(WithSpan {
                            span: rule.span,
                            node: Error::MultipleSkippingRule,
                        });
                        continue;
                    }
                    match construct_skip_rule(&ctx.definitions, rule) {
                        Ok(regex) => ctx.database.skip = Some(regex),
                        Err(e) => errs.extend(e),
                    }
                }
                LexicalTokenDef { name, .. } => {
                    match construct_lexical_rule(&ctx.definitions, rule) {
                        Ok((symbol, regex)) => {
                            let span = regex.span;
                            match ctx.database.symbol_table.insert(symbol.name(), name.span) {
                                None => {
                                    ctx.database.entries.insert(symbol, regex);
                                }
                                Some(previous) => {
                                    errs.push(WithSpan {
                                        span,
                                        node: Error::MultipleDefinition(symbol.name(), previous),
                                    });
                                }
                            }
                        }
                        Err(e) => errs.extend(e),
                    }
                }
                _ => {}
            }
        }
        if !errs.is_empty() {
            return Err(errs);
        }
        Ok(ctx.database)
    }
}
