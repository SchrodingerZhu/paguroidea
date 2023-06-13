// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;

use crate::{
    core_syntax::BindingContext,
    core_syntax::{ParserRule, Term, TermArena, TermPtr},
    nf::Tag,
    span_errors,
    type_system::{type_check, TypeError},
    utilities::{unreachable_branch, Symbol},
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
    pub symbol_table: HashMap<&'src str, WithSpan<'src, Symbol<'src>>>,
    pub lexer_database: LexerDatabase<'src>,
}

impl<'src, 'a> Parser<'src, 'a> {
    pub fn type_check(&self) -> Vec<TypeError<'src>> {
        let target = unsafe { self.bindings.get(&self.entrypoint).unwrap_unchecked() };
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
        ParserDef { entrypoint, rules } => {
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
                    ParserDefinition { active, name, expr } => {
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
                    ParserFixpoint { active, name, expr } => {
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
                    _ => unreachable_branch!(
                        "parser rule should only contains definitions or fixpoints"
                    ),
                }
            }
            parser.entrypoint = entrypoint.node;
            if !errs.is_empty() {
                return Err(errs);
            }
        }
        _ => unreachable_branch!("sst should be a parser"),
    }
    Ok(parser)
}

fn construct_symbol_table<'src>(
    context: &mut Parser<'src, '_>,
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> FrontendResult<'src, ()> {
    match &sst.node {
        ParserDef { rules, .. } => {
            for rule in rules {
                match &rule.node {
                    ParserFixpoint { name, .. } | ParserDefinition { name, .. } => {
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
                    _ => unreachable_branch!(
                        "parser rule should only contains definitions or fixpoints"
                    ),
                }
            }
            Ok(())
        }
        _ => unreachable_branch!("sst should be a parser"),
    }
}

fn construct_core_syntax_tree<'src, 'a>(
    translation_context: &Parser<'src, 'a>,
    sst: &WithSpan<'src, SurfaceSyntaxTree<'src>>,
) -> FrontendResult<'src, TermPtr<'src, 'a>> {
    match &sst.node {
        ParserAlternative { lhs, rhs } => {
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
        ParserSequence { lhs, rhs } => {
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
        ParserStar { inner } => {
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
        ParserPlus { inner } => {
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
        ParserOptional { inner } => {
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
        Bottom => Ok(translation_context.arena.alloc(WithSpan {
            span: sst.span,
            node: crate::core_syntax::Term::Bottom,
        })),
        Empty => Ok(translation_context.arena.alloc(WithSpan {
            span: sst.span,
            node: crate::core_syntax::Term::Epsilon,
        })),
        ParserRuleRef { name } => {
            let name = name.span.as_str();
            match translation_context.symbol_table.get(name) {
                Some(target) => Ok(translation_context.arena.alloc(WithSpan {
                    span: sst.span,
                    node: crate::core_syntax::Term::ParserRef(target.node),
                })),
                None => Err(span_errors!(UndefinedParserRuleReference, sst.span, name,)),
            }
        }
        LexicalRuleRef { name } => {
            let name = name.span.as_str();
            match translation_context.lexer_database.symbol_table.get(name) {
                Some(target) => Ok(translation_context.arena.alloc(WithSpan {
                    span: sst.span,
                    node: crate::core_syntax::Term::LexerRef(*target),
                })),
                None => Err(span_errors!(UndefinedLexicalReference, sst.span, name,)),
            }
        }
        _ => unreachable_branch!(
            "core syntax tree construction should not be called on this node: {}",
            sst.span.as_str()
        ),
    }
}
