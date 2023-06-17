// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

mod core_syntax;
mod frontend;
mod fusion;
mod nf;
mod type_system;
mod utilities;

use ariadne::{Color, Label, Report, ReportKind, Source};
use proc_macro2::TokenStream;
use quote::format_ident;
use typed_arena::Arena;

use std::ops::Range;

use core_syntax::TermArena;
use frontend::{
    lexical::construct_lexer_database, syntax::construct_parser, FrontendError,
    GrammarDefinitionError,
};
use fusion::fusion_parser;
use nf::{
    fully_normalize, merge_inactive_rules, remove_unreachable_rules, semi_normalize, NormalForms,
    Tag,
};
use type_system::TypeError;
use utilities::unreachable_branch;

pub enum Error<'src> {
    GrammarDefinitionError(GrammarDefinitionError<'src>),
    FrontendErrors(Vec<FrontendError<'src>>),
    TypeErrors(Vec<TypeError<'src>>),
}

impl<'src> From<Vec<FrontendError<'src>>> for Error<'src> {
    fn from(errors: Vec<FrontendError<'src>>) -> Self {
        Error::FrontendErrors(errors)
    }
}

impl<'src> From<Vec<TypeError<'src>>> for Error<'src> {
    fn from(errors: Vec<TypeError<'src>>) -> Self {
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
                use GrammarDefinitionError::*;
                vec![match e {
                    SyntaxError(x) => {
                        let span = match x.location {
                            pest::error::InputLocation::Pos(x) => x..x+1,
                            pest::error::InputLocation::Span((x, y)) => x..y,
                        };
                        Report::build(ReportKind::Error, input_name, span.start)
                            .with_message("Syntax error in grammar definition")
                            .with_label(Label::new((input_name, span))
                                .with_message(format!("{}", x.variant.message()))
                                .with_color(Color::Red))
                            .finish()
                    },
                    FormatError { span, message } => {
                        Report::build(ReportKind::Error, input_name, span.start())
                            .with_message("Format error in grammar definition")
                            .with_label(Label::new((input_name, span.start()..span.end()))
                                .with_message(format!("{}", message))
                                .with_color(Color::Red))
                            .finish()
                    },
                    ParserLogicError(e) => {
                        Report::build(ReportKind::Error, input_name, 0)
                            .with_message(format!("Internal logical error when parsing grammar definition {}", e))
                            .finish()
                    },
                    UnexpectedEOI(e) => {
                        Report::build(ReportKind::Error, input_name, 0)
                            .with_message(format!("Internal logical error when parsing grammar definition, pest parser failed to give {}", e))
                            .finish()
                    },
                }]
            },
            Error::FrontendErrors(errors) => errors
                .iter()
                .map(|e|  {
                    use FrontendError::*;
                    match &e {
                        // InternalLogicalError(span, msg) => {
                        //     Report::build(ReportKind::Error, input_name, span.start())
                        //         .with_message("Internal logical error encountered")
                        //         .with_label(Label::new((input_name, span.start()..span.end()))
                        //             .with_message(msg)
                        //             .with_color(Color::Red))
                        //         .finish()
                        // },
                        MultipleDefinition(fst, snd) => {
                            Report::build(ReportKind::Error, input_name, snd.start())
                                .with_message(format!("Multiple definition of {}", fst.as_str()))
                                .with_label(Label::new((input_name, fst.start()..fst.end()))
                                    .with_message("first definition")
                                    .with_color(Color::Green))
                                .with_label(Label::new((input_name, snd.start()..snd.end()))
                                    .with_message("second definition")
                                    .with_color(Color::Blue))
                                .finish()
                        },
                        UndefinedLexicalRuleReference(span) => {
                            Report::build(ReportKind::Error, input_name, span.start())
                                .with_message("Undefined lexical rule reference")
                                .with_label(Label::new((input_name, span.start()..span.end()))
                                    .with_message(format!("lexcical rule {} is undefined", span.as_str()))
                                    .with_color(Color::Red))
                                .finish()
                        },
                        CyclicLexicalRuleReference(span) => {
                            Report::build(ReportKind::Error, input_name, span.start())
                                .with_message("Cyclic lexical rule reference")
                                .with_label(Label::new((input_name, span.start()..span.end()))
                                    .with_message("this reference causes cyclic dependency")
                                    .with_color(Color::Red))
                                .finish()
                        },
                        UndefinedParserRuleReference(span) => {
                            Report::build(ReportKind::Error, input_name, span.start())
                                .with_message("Undefined parser rule reference")
                                .with_label(Label::new((input_name, span.start()..span.end()))
                                    .with_message(format!("parser rule {} is undefined", span.as_str()))
                                    .with_color(Color::Red))
                                .finish()
                        },
                        MultipleSkippingRule(fst, snd) => {
                            Report::build(ReportKind::Error, input_name, snd.start())
                                .with_message("Skipping lexical rule is already defined")
                                .with_label(Label::new((input_name, fst.start()..fst.end()))
                                    .with_message("first definition")
                                    .with_color(Color::Green))
                                .with_label(Label::new((input_name, snd.start()..snd.end()))
                                    .with_message("second definition")
                                    .with_color(Color::Blue))
                                .finish()
                        },
                        NullableToken(name, span) => {
                            Report::build(ReportKind::Error, input_name, span.start())
                                .with_message("Nullable token detected")
                                .with_label(Label::new((input_name, span.start()..span.end()))
                                    .with_message(format!("token {name} is nullable"))
                                    .with_color(Color::Red))
                                .finish()
                        },
                    }
                })
                .collect::<Vec<_>>(),
            Error::TypeErrors(errors) => errors
                .iter()
                .map(|e| {
                    use TypeError::*;
                    match e {
                        SequentialUniquenessViolation { lhs, rhs, total } => {
                            Report::build(ReportKind::Error, input_name, total.start())
                                .with_message("When type checking a sequence of rules, the following rules are ambiguous")
                                .with_label(Label::new((input_name, lhs.0.start()..lhs.0.end()))
                                    .with_message(format!("type info for left-hand side: nullable: {}, first set: {{{}}}, follow set: {{{}}}",
                                    lhs.1.nullable, lhs.1.first.iter().map(|x|x.name()).collect::<Vec<_>>().join(", "),
                                    lhs.1.follow.iter().map(|x|x.name()).collect::<Vec<_>>().join(", ")
                                ))
                                    .with_color(Color::Green))
                                .with_label(Label::new((input_name, rhs.0.start()..rhs.0.end()))
                                    .with_message(format!("type info for right-hand side: nullable: {}, first set: {{{}}}, follow set: {{{}}}",
                                    rhs.1.nullable, rhs.1.first.iter().map(|x|x.name()).collect::<Vec<_>>().join(", "),
                                    rhs.1.follow.iter().map(|x|x.name()).collect::<Vec<_>>().join(", ")
                                ))
                                    .with_color(Color::Blue))
                                .finish()
                        },
                        DisjunctiveUniquenessViolation { lhs, rhs, total } => {
                            Report::build(ReportKind::Error, input_name, total.start())
                                .with_message("When type checking an alternation of rules, the following rules are ambiguous")
                                .with_label(Label::new((input_name, lhs.0.start()..lhs.0.end()))
                                    .with_message(format!("type info for left-hand side: nullable: {}, first set:  {{{}}}, follow set:  {{{}}}",
                                    lhs.1.nullable, lhs.1.first.iter().map(|x|x.name()).collect::<Vec<_>>().join(", "),
                                    lhs.1.follow.iter().map(|x|x.name()).collect::<Vec<_>>().join(", ")
                                ))
                                    .with_color(Color::Green))
                                .with_label(Label::new((input_name, rhs.0.start()..rhs.0.end()))
                                    .with_message(format!("type info for right-hand side: nullable: {}, first set: {{{}}}, follow set: {{{}}}",
                                    rhs.1.nullable, rhs.1.first.iter().map(|x|x.name()).collect::<Vec<_>>().join(", "),
                                    rhs.1.follow.iter().map(|x|x.name()).collect::<Vec<_>>().join(", ")
                                ))
                                    .with_color(Color::Blue))
                                .finish()
                        },
                        UnguardedFixpoint(sym, span) => {
                            Report::build(ReportKind::Error, input_name, span.start())
                                .with_message("Unguarded fixpoint")
                                .with_label(Label::new((input_name, span.start()..span.end()))
                                    .with_message(format!("fixpoint rule {} is not guarded -- your grammar is left-recursive", sym))
                                    .with_color(Color::Red))
                                .finish()
                        },
                        UnresolvedReference(sym, span) => {
                            Report::build(ReportKind::Error, input_name, span.start())
                                .with_message("Unresolved reference")
                                .with_label(Label::new((input_name, span.start()..span.end()))
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

    let sst = frontend::parse(input)?;
    let Grammar { lexer, parser } = &sst.node else {
        unreachable_branch!("the entrypoint of sst can only be Grammar")
    };
    let lexer_database = construct_lexer_database(lexer)?;
    lexer_database.nullability_check()?;
    let term_arena = TermArena::new();
    let mut parser = construct_parser(&term_arena, lexer_database, parser)?;
    parser.infer_fixpoints();
    let type_errs = parser.type_check();
    if !type_errs.is_empty() {
        return Err(Error::TypeErrors(type_errs));
    }
    let nf_arena = Arena::new();
    let mut nfs = NormalForms::new();
    for (symbol, rule) in parser.bindings.iter() {
        semi_normalize(
            &rule.term.node,
            Tag::new(*symbol),
            &nf_arena,
            &mut nfs,
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
            unused_parens,
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

#[cfg(test)]
mod tests;
