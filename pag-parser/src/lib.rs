// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::ops::Range;

use ariadne::{Color, Report, ReportKind, Source};
use fusion::fusion_parser;
use proc_macro2::TokenStream;
use quote::format_ident;
use typed_arena::Arena;
use utilities::unreachable_branch;

use crate::{
    core_syntax::TermArena,
    frontend::{
        fixpoint::infer_fixpoints, lexical::LexerDatabase, syntax::construct_parser,
        FrontendErrors, GrammarDefinitionError,
    },
    nf::{
        fully_normalize, merge_inactive_rules, remove_unreachable_rules, semi_normalize,
        NormalForms, Tag, TagAssigner,
    },
};

pub mod core_syntax;
pub mod frontend;
mod fusion;
mod nf;
pub mod type_system;
pub mod utilities;

pub enum Error<'src> {
    GrammarDefinitionError(GrammarDefinitionError<'src>),
    FrontendErrors(FrontendErrors<'src>),
    TypeErrors(Vec<type_system::TypeError<'src>>),
}

impl<'src> From<FrontendErrors<'src>> for Error<'src> {
    fn from(errors: FrontendErrors<'src>) -> Self {
        Error::FrontendErrors(errors)
    }
}

impl<'src> From<Vec<type_system::TypeError<'src>>> for Error<'src> {
    fn from(errors: Vec<type_system::TypeError<'src>>) -> Self {
        Error::TypeErrors(errors)
    }
}

impl<'src> Error<'src> {
    pub fn report_stderr(&self, input_name: &str, input: &'src str) -> Result<(), std::io::Error> {
        let mut cache = (input_name, Source::from(input));
        for i in self.to_reports(input_name) {
            i.eprint(&mut cache)?;
        }
        Ok(())
    }
    pub fn report_stdout(&self, input_name: &str, input: &'src str) -> Result<(), std::io::Error> {
        let mut cache = (input_name, Source::from(input));
        for i in self.to_reports(input_name) {
            i.print(&mut cache)?;
        }
        Ok(())
    }
    pub fn to_reports<'a>(&self, input_name: &'a str) -> Vec<Report<'a, (&'a str, Range<usize>)>> {
        match self {
            Error::GrammarDefinitionError(e) => {
                vec![match e {
                    GrammarDefinitionError::SyntaxError(x) => {
                        let span = match x.location {
                            pest::error::InputLocation::Pos(x) => x..x+1,
                            pest::error::InputLocation::Span((x, y)) => x..y,
                        };
                        Report::build(ReportKind::Error, input_name, span.start)
                            .with_message("Syntax error in grammar definition")
                            .with_label(ariadne::Label::new((input_name, span))
                                .with_message(format!("{}", x.variant.message()))
                                .with_color(Color::Red))
                            .finish()
                    },
                    GrammarDefinitionError::FormatError { span, message } => {
                        Report::build(ReportKind::Error, input_name, span.start())
                            .with_message("Format error in grammar definition")
                            .with_label(ariadne::Label::new((input_name, span.start()..span.end()))
                                .with_message(format!("{}", message))
                                .with_color(Color::Red))
                            .finish()
                    },
                    GrammarDefinitionError::ParserLogicError(e) => {
                        Report::build(ReportKind::Error, input_name, 0)
                            .with_message(format!("Internal logical error when parsing grammar definition {}", e))
                            .finish()
                    },
                    GrammarDefinitionError::UnexpectedEOI(e) => {
                        Report::build(ReportKind::Error, input_name, 0)
                            .with_message(format!("Internal logical error when parsing grammar definition, pest parser failed to give {}", e))
                            .finish()
                    },
                }]
            },
            Error::FrontendErrors(errors) => errors
                .iter()
                .map(|e|  {
                    match &e.node {
                        frontend::Error::InternalLogicalError(msg) => {
                            Report::build(ReportKind::Error, input_name, e.span.start())
                                .with_message("Internal logical error encountered")
                                .with_label(ariadne::Label::new((input_name, e.span.start()..e.span.end()))
                                    .with_message(msg)
                                    .with_color(Color::Red))
                                .finish()
                        },
                        frontend::Error::MultipleDefinition(x, y) => {
                            Report::build(ReportKind::Error, input_name, e.span.start().min(y.start()))
                                .with_message(format!("Multiple definition of {}", x))
                                .with_label(ariadne::Label::new((input_name, y.start()..y.end()))
                                    .with_message("first definition")
                                    .with_color(Color::Green))
                                .with_label(ariadne::Label::new((input_name, e.span.start()..e.span.end()))
                                    .with_message("second definition")
                                    .with_color(Color::Blue))
                                .finish()
                        },
                        frontend::Error::InvalidLexicalReference(name) => {
                            Report::build(ReportKind::Error, input_name, e.span.start())
                                .with_message("Invalid lexical reference")
                                .with_label(ariadne::Label::new((input_name, e.span.start()..e.span.end()))
                                    .with_message(format!("referencing lexical rule {} is not allowed here", name))
                                    .with_color(Color::Red))
                                .finish()
                        },
                        frontend::Error::UndefinedLexicalReference(name) => {
                            Report::build(ReportKind::Error, input_name, e.span.start())
                                .with_message("Undefined lexical reference")
                                .with_label(ariadne::Label::new((input_name, e.span.start()..e.span.end()))
                                    .with_message(format!("lexcical rule {} is undefined", name))
                                    .with_color(Color::Red))
                                .finish()
                        },
                        frontend::Error::UndefinedParserRuleReference(name) => {
                            Report::build(ReportKind::Error, input_name, e.span.start())
                                .with_message("Undefined parser rule reference")
                                .with_label(ariadne::Label::new((input_name, e.span.start()..e.span.end()))
                                    .with_message(format!("parser rule {} is undefined", name))
                                    .with_color(Color::Red))
                                .finish()
                        },
                        frontend::Error::MultipleSkippingRule(name) => {
                            Report::build(ReportKind::Error, input_name, e.span.start())
                                .with_message("Skipping lexical rule is already defined")
                                .with_label(ariadne::Label::new((input_name, e.span.start()..e.span.end()))
                                    .with_message(format!("this definition conflicts with previous definition for {name}"))
                                    .with_color(Color::Red))
                                .finish()
                        },
                        frontend::Error::NullableToken(name) => {
                            Report::build(ReportKind::Error, input_name, e.span.start())
                                .with_message("Nullable token detected")
                                .with_label(ariadne::Label::new((input_name, e.span.start()..e.span.end()))
                                    .with_message(format!("token {} is nullable", name))
                                    .with_color(Color::Red))
                                .finish()
                        },
                    }
                })
                .collect::<Vec<_>>(),
            Error::TypeErrors(errors) => errors
                .iter()
                .map(|e| {
                    match e {
                        type_system::TypeError::SequentialUniquenessViolation { lhs, rhs, total } => {
                            Report::build(ReportKind::Error, input_name, total.start())
                                .with_message("When type checking a sequence of rules, the following rules are ambiguous")
                                .with_label(ariadne::Label::new((input_name, lhs.0.start()..lhs.0.end()))
                                    .with_message(format!("type info for left-hand side: nullable: {}, first set: {{{}}}, follow set: {{{}}}",
                                    lhs.1.nullable, lhs.1.first.iter().map(|x|x.name()).collect::<Vec<_>>().join(", "),
                                    lhs.1.follow.iter().map(|x|x.name()).collect::<Vec<_>>().join(", ")
                                ))
                                    .with_color(Color::Green))
                                .with_label(ariadne::Label::new((input_name, rhs.0.start()..rhs.0.end()))
                                    .with_message(format!("type info for right-hand side: nullable: {}, first set: {{{}}}, follow set: {{{}}}",
                                    rhs.1.nullable, rhs.1.first.iter().map(|x|x.name()).collect::<Vec<_>>().join(", "),
                                    rhs.1.follow.iter().map(|x|x.name()).collect::<Vec<_>>().join(", ")
                                ))
                                    .with_color(Color::Blue))
                                .finish()
                        },
                        type_system::TypeError::DisjunctiveUniquenessViolation { lhs, rhs, total } => {
                            Report::build(ReportKind::Error, input_name, total.start())
                                .with_message("When type checking an alternation of rules, the following rules are ambiguous")
                                .with_label(ariadne::Label::new((input_name, lhs.0.start()..lhs.0.end()))
                                    .with_message(format!("type info for left-hand side: nullable {}, first set: {}, follow set: {}",
                                    lhs.1.nullable, lhs.1.first.iter().map(|x|x.name()).collect::<Vec<_>>().join(", "),
                                    lhs.1.follow.iter().map(|x|x.name()).collect::<Vec<_>>().join(", ")
                                ))
                                    .with_color(Color::Green))
                                .with_label(ariadne::Label::new((input_name, rhs.0.start()..rhs.0.end()))
                                    .with_message(format!("type info for right-hand side: nullable {}, first set: {}, follow set: {}",
                                    rhs.1.nullable, rhs.1.first.iter().map(|x|x.name()).collect::<Vec<_>>().join(", "),
                                    rhs.1.follow.iter().map(|x|x.name()).collect::<Vec<_>>().join(", ")
                                ))
                                    .with_color(Color::Blue))
                                .finish()
                        },
                        type_system::TypeError::UnguardedFixpoint(sym, s) => {
                            Report::build(ReportKind::Error, input_name, s.start())
                                .with_message("Unguarded fixpoint")
                                .with_label(ariadne::Label::new((input_name,s.start()..s.end()))
                                    .with_message(format!("fixpoint rule {} is not guarded -- your grammar is left-recursive", sym))
                                    .with_color(Color::Red))
                                .finish()
                        },
                        type_system::TypeError::UnresolvedReference(sym, s) => {
                            Report::build(ReportKind::Error, input_name, s.start())
                                .with_message("Unresolved reference")
                                .with_label(ariadne::Label::new((input_name,s.start()..s.end()))
                                    .with_message(format!("cannot resolve parser rule {} within context -- did you forget to put recursive rule into fixpoint", sym))
                                    .with_color(Color::Red))
                                .finish()
                        },
                    }
                })
                .collect::<Vec<_>>(),
        }
    }
}

pub fn generate_parser(input: &str) -> Result<TokenStream, Error> {
    use frontend::SurfaceSyntaxTree::Grammar;

    let mut sst = frontend::parse(input)?;
    let Grammar { lexer, parser } = &mut sst.node else {
        unreachable_branch!("the entrypoint of sst can only be Grammar")
    };
    infer_fixpoints(parser)?;
    let lexer_database = LexerDatabase::new(lexer)?;
    let nullable_errors = lexer_database.nullability_check();
    if !nullable_errors.is_empty() {
        return Err(Error::FrontendErrors(nullable_errors));
    }
    let term_arena = TermArena::new();
    let parser = construct_parser(&term_arena, lexer_database, parser)?;
    let type_errs = parser.type_check();
    if !type_errs.is_empty() {
        return Err(Error::TypeErrors(type_errs));
    }
    let nf_arena = Arena::new();
    let mut nfs = NormalForms::new();
    let mut assigner = TagAssigner::new();
    for (i, rule) in parser.bindings.iter() {
        semi_normalize(
            &rule.term.node,
            Tag::new(*i),
            &nf_arena,
            &mut nfs,
            &mut assigner,
            &parser,
        );
    }
    fully_normalize(&nf_arena, &mut nfs);
    merge_inactive_rules(&mut nfs, &parser, &nf_arena);
    remove_unreachable_rules(&mut nfs, &parser);
    let parser_routines = fusion_parser(&nfs, &parser);
    let entrypoint = format_ident!("parse_{}", parser.entrypoint.name());
    Ok(quote::quote! {
        #![allow(
            non_snake_case,
            dead_code,
            non_camel_case_types,
            unused_variables,
            unused_mut,
            unreachable_patterns,
            unreachable_code,
            unused_assignments,
            clippy::identity_op,
            clippy::single_match,
            clippy::never_loop,
            clippy::match_single_binding,
        )]
        #parser_routines
        pub fn parse(input: &str) -> Result<ParserTree, Error> {
            #entrypoint(input, 0)
        }
    })
}
