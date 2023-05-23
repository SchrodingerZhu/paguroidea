// Copyright (c) 2023 Paguroidea Developpers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{collections::HashMap, rc::Rc};

use pag_lexer::{normalization::normalize, regex_tree::RegexTree};

use crate::span_errors;
use crate::{utilities::unreachable_branch, utilities::Symbol};

use super::{SurfaceSyntaxTree, WithSpan};

use super::Error;

type SpanRegexTree<'a> = WithSpan<'a, Rc<RegexTree>>;

pub struct LexerRule<'a> {
    pub active: bool,
    pub rule: SpanRegexTree<'a>,
}

// todo: check only one skip rule
pub struct LexerDatabase<'a> {
    pub symbol_table: HashMap<&'a str, Symbol<'a>>,
    pub entries: HashMap<Symbol<'a>, LexerRule<'a>>,
    pub skip: Option<Symbol<'a>>,
}

impl<'a> LexerDatabase<'a> {
    pub fn new(
        sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
    ) -> Result<Self, Vec<WithSpan<'a, Error<'a>>>> {
        TranslationContext::create_database(sst)
    }
}

struct TranslationContext<'a> {
    definitions: HashMap<&'a str, SpanRegexTree<'a>>,
    database: LexerDatabase<'a>,
}

fn construct_regex_tree<'a, F>(
    sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
    reference_handler: &F,
) -> Result<Rc<RegexTree>, Vec<WithSpan<'a, Error<'a>>>>
where
    F: Fn(WithSpan<'a, ()>) -> Result<Rc<RegexTree>, Vec<WithSpan<'a, Error<'a>>>>,
{
    match &sst.node {
        SurfaceSyntaxTree::LexicalAlternative { lhs, rhs } => {
            let lhs = construct_regex_tree(lhs, reference_handler);
            let rhs = construct_regex_tree(rhs, reference_handler);
            match (lhs, rhs) {
                (Err(mut lhs), Err(rhs)) => {
                    lhs.extend(rhs.into_iter());
                    Err(lhs)
                }
                (Err(lhs), _) => Err(lhs),
                (_, Err(rhs)) => Err(rhs),
                (Ok(lhs), Ok(rhs)) => Ok(Rc::new(RegexTree::Union(lhs, rhs))),
            }
        }
        SurfaceSyntaxTree::LexicalSequence { lhs, rhs } => {
            let lhs = construct_regex_tree(lhs, reference_handler);
            let rhs = construct_regex_tree(rhs, reference_handler);
            match (lhs, rhs) {
                (Err(mut lhs), Err(rhs)) => {
                    lhs.extend(rhs.into_iter());
                    Err(lhs)
                }
                (Err(lhs), _) => Err(lhs),
                (_, Err(rhs)) => Err(rhs),
                (Ok(lhs), Ok(rhs)) => Ok(Rc::new(RegexTree::Concat(lhs, rhs))),
            }
        }
        SurfaceSyntaxTree::LexicalStar { inner } => {
            let inner = construct_regex_tree(inner, reference_handler)?;
            Ok(Rc::new(RegexTree::KleeneClosure(inner)))
        }
        SurfaceSyntaxTree::LexicalPlus { inner } => {
            let inner = construct_regex_tree(inner, reference_handler)?;
            Ok(Rc::new(RegexTree::Concat(
                inner.clone(),
                Rc::new(RegexTree::KleeneClosure(inner)),
            )))
        }
        SurfaceSyntaxTree::LexicalOptional { inner } => {
            let inner = construct_regex_tree(inner, reference_handler)?;
            Ok(Rc::new(RegexTree::Union(
                inner,
                Rc::new(RegexTree::Epsilon),
            )))
        }
        SurfaceSyntaxTree::LexicalNot { inner } => {
            let inner = construct_regex_tree(inner, reference_handler)?;
            Ok(Rc::new(RegexTree::Complement(inner)))
        }
        SurfaceSyntaxTree::Range { start, end } => Ok(Rc::new(RegexTree::range(*start..=*end))),
        SurfaceSyntaxTree::String(x) => {
            if x.is_empty() {
                Ok(Rc::new(RegexTree::Epsilon))
            } else {
                let mut iter = x.chars();
                let fst = Rc::new(RegexTree::single(unsafe { iter.next().unwrap_unchecked() }));
                Ok(iter.fold(fst, |acc, c| {
                    Rc::new(RegexTree::Concat(acc, Rc::new(RegexTree::single(c))))
                }))
            }
        }
        SurfaceSyntaxTree::Bottom => Ok(Rc::new(RegexTree::Bottom)),
        SurfaceSyntaxTree::Empty => Ok(Rc::new(RegexTree::Epsilon)),
        SurfaceSyntaxTree::Char { value } => Ok(Rc::new(RegexTree::single(value.node))),
        SurfaceSyntaxTree::LexicalRuleRef { name } => reference_handler(name.clone()),
        _ => unreachable_branch!(
            "lexer translation is called with unsupported code: {}",
            sst.span.as_str()
        ),
    }
}

fn construct_definition<'a>(
    sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
) -> Result<(&'a str, SpanRegexTree<'a>), Vec<WithSpan<'a, Error<'a>>>> {
    fn reference_handler(
        name: WithSpan<'_, ()>,
    ) -> Result<Rc<RegexTree>, Vec<WithSpan<'_, Error<'_>>>> {
        Err(span_errors!(
            InvalidLexicalReference,
            name.span,
            name.span.as_str(),
        ))
    }
    match &sst.node {
        SurfaceSyntaxTree::LexicalDefinition { name, expr } => {
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
        _ => unreachable_branch!(
            "lexer translation can only be called with lexical definition: {}",
            sst.span.as_str()
        ),
    }
}

fn construct_lexical_rule<'a>(
    definitions: &HashMap<&'a str, SpanRegexTree<'a>>,
    sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
) -> Result<(Symbol<'a>, SpanRegexTree<'a>), Vec<WithSpan<'a, Error<'a>>>> {
    let reference_handler = |name: WithSpan<'a, ()>| match definitions.get(name.span.as_str()) {
        Some(x) => Ok(x.node.clone()),
        _ => Err(span_errors!(
            UndefinedLexicalReference,
            name.span,
            name.span.as_str(),
        )),
    };
    match &sst.node {
        SurfaceSyntaxTree::LexicalToken { name, expr, .. } => {
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
        _ => unreachable_branch!(
            "lexer rule translation can only be called with lexical definition: {}",
            sst.span.as_str()
        ),
    }
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
    ) -> Result<(), Vec<WithSpan<'a, Error<'a>>>> {
        match &sst.node {
            SurfaceSyntaxTree::Lexer { rules } => {
                let mut error = Vec::new();
                for i in rules
                    .iter()
                    .filter(|x| matches!(x.node, SurfaceSyntaxTree::LexicalDefinition { .. }))
                    .map(construct_definition)
                {
                    match i {
                        Err(e) => {
                            error.extend(e.into_iter());
                        }
                        Ok((k, v)) => {
                            let current_span = v.span;
                            match self.definitions.insert(k, v) {
                                None => (),
                                Some(x) => {
                                    error.push(WithSpan {
                                        span: current_span,
                                        node: Error::MultipleDefinition(k, x.span),
                                    });
                                }
                            }
                        }
                    }
                }

                if error.is_empty() {
                    Ok(())
                } else {
                    Err(error)
                }
            }
            _ => Err(span_errors!(
                InternalLogicalError,
                sst.span,
                "populating definitions from a non-lexer node".into(),
            )),
        }
    }
    fn create_database(
        sst: &WithSpan<'a, SurfaceSyntaxTree<'a>>,
    ) -> Result<LexerDatabase<'a>, Vec<WithSpan<'a, Error<'a>>>> {
        let mut ctx = Self::new();
        let mut errs = match ctx.populate_definitions(sst) {
            Ok(_) => Vec::new(),
            Err(errs) => errs,
        };
        match &sst.node {
            SurfaceSyntaxTree::Lexer { rules } => {
                for i in rules
                    .iter()
                    .filter_map(|x| match &x.node {
                        SurfaceSyntaxTree::LexicalToken { active, .. } => Some((*active, x)),
                        _ => None,
                    })
                    .map(|(active, sst)| (active, construct_lexical_rule(&ctx.definitions, sst)))
                {
                    match i.1 {
                        Err(e) => {
                            errs.extend(e.into_iter());
                        }
                        Ok((k, rule)) => {
                            if !i.0 {
                                // TODO: make sure only one skip rule is defined
                                ctx.database.skip.replace(k);
                            }
                            let current_span = rule.span;
                            match ctx.database.symbol_table.insert(k.name(), k) {
                                None => {
                                    ctx.database
                                        .entries
                                        .insert(k, LexerRule { active: i.0, rule });
                                }
                                Some(x) => {
                                    errs.push(WithSpan {
                                        span: current_span,
                                        node: Error::MultipleDefinition(k.name(), unsafe {
                                            ctx.database
                                                .entries
                                                .get(&x)
                                                .unwrap_unchecked()
                                                .rule
                                                .span
                                        }),
                                    });
                                }
                            }
                        }
                    }
                }

                if errs.is_empty() {
                    Ok(ctx.database)
                } else {
                    Err(errs)
                }
            }
            _ => Err(span_errors!(
                InternalLogicalError,
                sst.span,
                "creating lexer rule database from a non-lexer node".into(),
            )),
        }
    }
}
