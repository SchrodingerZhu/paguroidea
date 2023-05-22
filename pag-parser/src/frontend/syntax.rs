// Copyright (c) 2023 Paguroidea Developpers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::{collections::HashMap, todo, unimplemented};

use crate::{
    core_syntax::BindingContext,
    core_syntax::{ParserRule, Term, TermArena, TermPtr},
    nf::Tag,
    span_errors,
    type_system::{type_check, TypeError},
    unreachable_branch,
    utilities::Symbol,
};

use super::{lexical::LexerDatabase, Error, SurfaceSyntaxTree, WithSpan};

pub struct Parser<'src, 'a> {
    pub entrypoint: Symbol<'src>,
    pub arena: &'a TermArena<'src, 'a>,
    pub bindings: BindingContext<'src, 'a>,
    pub symbol_table: HashMap<&'src str, WithSpan<'src, Symbol<'src>>>,
    pub lexer_database: LexerDatabase<'src>,
}

impl<'src, 'a> Parser<'src, 'a> {
    pub fn type_check(&self) -> Vec<TypeError<'src>> {
        let target = unsafe { self.bindings.get(&self.entrypoint).unwrap_unchecked() };
        type_check(&self.bindings, target.term)
    }
    pub fn is_active(&self, tag: &Tag<'src>) -> bool {
        !tag.is_versioned()
            && self
                .bindings
                .get(&tag.symbol())
                .map(|x| x.active)
                .unwrap_or(false)
    }
}

pub fn construct_parser<'src, 'a>(
    arena: &'a TermArena<'src, 'a>,
    lexer_database: LexerDatabase<'src>,
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> Result<Parser<'src, 'a>, Vec<WithSpan<'src, Error<'src>>>> {
    let mut parser = Parser {
        entrypoint: Symbol::new(""),
        arena,
        bindings: HashMap::new(),
        lexer_database,
        symbol_table: HashMap::new(),
    };
    let mut errs = match construct_symbol_table(&mut parser, sst) {
        Ok(()) => vec![],
        Err(errs) => errs,
    };
    match &sst.node {
        SurfaceSyntaxTree::Parser { entrypoint, rules } => {
            let entrypoint = match parser.symbol_table.get(entrypoint.span.as_str()) {
                Some(entrypoint) => entrypoint.clone(),
                None => {
                    errs.extend(span_errors!(
                        UndefinedParserRuleReference,
                        entrypoint.span,
                        entrypoint.span.as_str(),
                    ));
                    return Err(errs);
                }
            };
            for i in rules.iter() {
                match &i.node {
                    SurfaceSyntaxTree::ParserDefinition { active, name, expr } => {
                        match construct_core_syntax_tree(&parser, expr) {
                            Ok(expr) => {
                                let name = unsafe {
                                    parser
                                        .symbol_table
                                        .get(name.span.as_str())
                                        .unwrap_unchecked()
                                };
                                parser.bindings.insert(
                                    name.node,
                                    ParserRule {
                                        active: *active,
                                        term: parser.arena.alloc(WithSpan {
                                            span: i.span,
                                            node: expr.node.clone(),
                                        }),
                                    },
                                );
                            }
                            Err(e) => errs.extend(e),
                        }
                    }
                    SurfaceSyntaxTree::ParserFixpoint { active, name, expr } => {
                        match construct_core_syntax_tree(&parser, expr) {
                            Ok(expr) => {
                                let name = unsafe {
                                    parser
                                        .symbol_table
                                        .get(name.span.as_str())
                                        .unwrap_unchecked()
                                };
                                parser.bindings.insert(
                                    name.node,
                                    ParserRule {
                                        active: *active,
                                        term: parser.arena.alloc(WithSpan {
                                            span: i.span,
                                            node: Term::Fix(name.node, expr),
                                        }),
                                    },
                                );
                            }
                            Err(e) => errs.extend(e),
                        }
                    }
                    _ => unreachable_branch(),
                }
            }
            parser.entrypoint = entrypoint.node;
            if !errs.is_empty() {
                return Err(errs);
            }
        }
        _ => unreachable_branch(),
    }
    Ok(parser)
}

fn construct_symbol_table<'src>(
    context: &mut Parser<'src, '_>,
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> Result<(), Vec<WithSpan<'src, Error<'src>>>> {
    match &sst.node {
        SurfaceSyntaxTree::Parser { rules, .. } => {
            for rule in rules {
                match &rule.node {
                    SurfaceSyntaxTree::ParserFixpoint { name, .. }
                    | SurfaceSyntaxTree::ParserDefinition { name, .. } => {
                        if let Some(previous) = context.symbol_table.get(name.span.as_str()) {
                            return Err(span_errors!(
                                MultipleDefinition,
                                name.span,
                                name.span.as_str(),
                                previous.span,
                            ));
                        } else {
                            context.symbol_table.insert(
                                name.span.as_str(),
                                WithSpan {
                                    span: name.span,
                                    node: Symbol::new(name.span.as_str()),
                                },
                            );
                        }
                    }
                    _ => unreachable_branch(),
                }
            }
            Ok(())
        }
        _ => unreachable_branch(),
    }
}

fn construct_core_syntax_tree<'src, 'a>(
    translation_context: &Parser<'src, 'a>,
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> Result<TermPtr<'src, 'a>, Vec<WithSpan<'src, Error<'src>>>> {
    match &sst.node {
        SurfaceSyntaxTree::ParserAlternative { lhs, rhs } => {
            let lhs = construct_core_syntax_tree(translation_context, lhs);
            let rhs = construct_core_syntax_tree(translation_context, rhs);
            match (lhs, rhs) {
                (Ok(lhs), Ok(rhs)) => Ok(translation_context.arena.alloc(WithSpan {
                    span: sst.span,
                    node: crate::core_syntax::Term::Alternative(lhs, rhs),
                })),
                (Ok(_), Err(rhs)) => Err(rhs),
                (Err(lhs), Ok(_)) => Err(lhs),
                (Err(mut lhs), Err(mut rhs)) => {
                    lhs.append(&mut rhs);
                    Err(lhs)
                }
            }
        }
        SurfaceSyntaxTree::ParserSequence { lhs, rhs } => {
            let lhs = construct_core_syntax_tree(translation_context, lhs);
            let rhs = construct_core_syntax_tree(translation_context, rhs);
            match (lhs, rhs) {
                (Ok(lhs), Ok(rhs)) => Ok(translation_context.arena.alloc(WithSpan {
                    span: sst.span,
                    node: crate::core_syntax::Term::Sequence(lhs, rhs),
                })),
                (Ok(_), Err(rhs)) => Err(rhs),
                (Err(lhs), Ok(_)) => Err(lhs),
                (Err(mut lhs), Err(mut rhs)) => {
                    lhs.append(&mut rhs);
                    Err(lhs)
                }
            }
        }
        SurfaceSyntaxTree::ParserStar { inner } => {
            let symbol = Symbol::new(sst.span.as_str());
            let inner = construct_core_syntax_tree(translation_context, inner)?;
            // \x . (i ~ x) | epsilon
            let sequence = translation_context.arena.alloc(WithSpan {
                span: sst.span,
                node: crate::core_syntax::Term::Sequence(
                    inner,
                    translation_context.arena.alloc(WithSpan {
                        span: sst.span,
                        node: crate::core_syntax::Term::ParserRef(symbol),
                    }),
                ),
            });
            let alternative = translation_context.arena.alloc(WithSpan {
                span: sst.span,
                node: crate::core_syntax::Term::Alternative(
                    sequence,
                    translation_context.arena.alloc(WithSpan {
                        span: sst.span,
                        node: crate::core_syntax::Term::Epsilon,
                    }),
                ),
            });
            Ok(translation_context.arena.alloc(WithSpan {
                span: sst.span,
                node: crate::core_syntax::Term::Fix(symbol, alternative),
            }))
        }
        SurfaceSyntaxTree::ParserPlus { inner } => {
            let symbol = Symbol::new(sst.span.as_str());
            let inner = construct_core_syntax_tree(translation_context, inner)?;
            // \x . (i ~ x) | epsilon
            let sequence = translation_context.arena.alloc(WithSpan {
                span: sst.span,
                node: crate::core_syntax::Term::Sequence(
                    inner,
                    translation_context.arena.alloc(WithSpan {
                        span: sst.span,
                        node: crate::core_syntax::Term::ParserRef(symbol),
                    }),
                ),
            });
            let alternative = translation_context.arena.alloc(WithSpan {
                span: sst.span,
                node: crate::core_syntax::Term::Alternative(
                    sequence,
                    translation_context.arena.alloc(WithSpan {
                        span: sst.span,
                        node: crate::core_syntax::Term::Epsilon,
                    }),
                ),
            });
            let fixpoint = translation_context.arena.alloc(WithSpan {
                span: sst.span,
                node: crate::core_syntax::Term::Fix(symbol, alternative),
            });
            Ok(translation_context.arena.alloc(WithSpan {
                span: sst.span,
                node: crate::core_syntax::Term::Sequence(inner, fixpoint),
            }))
        }
        SurfaceSyntaxTree::ParserOptional { inner } => {
            let inner = construct_core_syntax_tree(translation_context, inner)?;
            Ok(translation_context.arena.alloc(WithSpan {
                span: sst.span,
                node: crate::core_syntax::Term::Alternative(
                    inner,
                    translation_context.arena.alloc(WithSpan {
                        span: sst.span,
                        node: crate::core_syntax::Term::Epsilon,
                    }),
                ),
            }))
        }
        SurfaceSyntaxTree::Bottom => Ok(translation_context.arena.alloc(WithSpan {
            span: sst.span,
            node: crate::core_syntax::Term::Bottom,
        })),
        SurfaceSyntaxTree::Empty => Ok(translation_context.arena.alloc(WithSpan {
            span: sst.span,
            node: crate::core_syntax::Term::Epsilon,
        })),
        SurfaceSyntaxTree::ParserRuleRef { name } => {
            let name = name.span.as_str();
            match translation_context.symbol_table.get(name) {
                Some(target) => Ok(translation_context.arena.alloc(WithSpan {
                    span: sst.span,
                    node: crate::core_syntax::Term::ParserRef(target.node),
                })),
                None => Err(span_errors!(UndefinedParserRuleReference, sst.span, name,)),
            }
        }
        SurfaceSyntaxTree::LexicalRuleRef { name } => {
            let name = name.span.as_str();
            match translation_context.lexer_database.symbol_table.get(name) {
                Some(target) => Ok(translation_context.arena.alloc(WithSpan {
                    span: sst.span,
                    node: crate::core_syntax::Term::LexerRef(*target),
                })),
                None => Err(span_errors!(UndefinedLexicalReference, sst.span, name,)),
            }
        }
        _ => unreachable_branch(),
    }
}