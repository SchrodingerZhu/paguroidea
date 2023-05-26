// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use lazy_static::lazy_static;
use pest::iterators::Pair;
use pest::pratt_parser::{Op, PrattParser};
use pest::Span;
use std::borrow::Cow;
use std::fmt::Display;
use thiserror::Error;
pub mod lexical;
pub mod syntax;

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error<'a> {
    #[error("internal logic error: {0}")]
    InternalLogicalError(Cow<'a, str>),
    #[error("multiple definition for {0}")]
    MultipleDefinition(&'a str, pest::Span<'a>),
    #[error("lexical reference {0} is not allowed within lexical definitions")]
    InvalidLexicalReference(&'a str),
    #[error("multiple skip rule detected, previous definition is {0}")]
    MultipleSkippingRule(&'a str),
    #[error("nullable token {0} is prohibited")]
    NullableToken(&'a str),
    #[error("lexical {0} is undefined")]
    UndefinedLexicalReference(&'a str),
    #[error("parser rule {0} is undefined")]
    UndefinedParserRuleReference(&'a str),
}

#[macro_export]
macro_rules! span_errors {
    ($ekind:ident, $span:expr, $($params:expr,)*) => {
        vec![WithSpan {
            span: $span,
            node: Error::$ekind ($($params,)*)
        }]
    };
}

macro_rules! unexpected_eoi {
    ($expectation:literal) => {
        $crate::frontend::GrammarDefinitionError::UnexpectedEOI(
            $expectation.into(),
        )
    };
    ($format:literal, $($arg:tt)+) => {
        $crate::frontend::GrammarDefinitionError::UnexpectedEOI(
            format!($format, $($arg)+).into(),
        )
    };
}
macro_rules! parser_logical_error {
    ($expectation:expr) => {
        $crate::frontend::GrammarDefinitionError::ParserLogicError(
            $expectation.into(),
        )
    };
    ($format:literal, $($arg:tt)+) => {
        $crate::frontend::GrammarDefinitionError::ParserLogicError(
            format!($format, $($arg)+).into(),
        )
    };
}

macro_rules! format_error {
    ($span:expr, $message:expr) => {
        $crate::frontend::GrammarDefinitionError::FormatError {
            span: $span,
            message: $message.into(),
        }
    };
    ($span:expr, $format:literal, $($arg:tt)+) => {
        $crate::frontend::GrammarDefinitionError::FormatError {
            span: $span,
            message: format!($format, $($arg)+),
        }
    };
}

mod grammar {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "src/frontend/grammar.pest"]
    pub struct Parser;
}

use crate::utilities::unreachable_branch;
pub use grammar::Parser as GrammarParser;
pub use grammar::Rule;

#[derive(Debug, Error, Clone)]
pub enum GrammarDefinitionError<'a> {
    #[error("grammar definition error: {0}")]
    SyntaxError(#[from] Box<pest::error::Error<Rule>>),
    #[error("failed to parse {}: {message}", span.as_str())]
    FormatError { span: Span<'a>, message: String },
    #[error("{0}")]
    ParserLogicError(Cow<'a, str>),
    #[error("unexpected end of input, expecting {0}")]
    UnexpectedEOI(Cow<'a, str>),
}

lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::Assoc::*;

        PrattParser::new()
            .op(Op::infix(Rule::lexical_alternative, Left))
            .op(Op::infix(Rule::lexical_sequence, Left))
            .op(Op::postfix(Rule::lexical_star)
                | Op::postfix(Rule::lexical_plus)
                | Op::postfix(Rule::lexical_optional)
                | Op::prefix(Rule::lexical_not))
            .op(Op::infix(Rule::parser_alternative, Left))
            .op(Op::infix(Rule::parser_sequence, Left))
            .op(Op::postfix(Rule::parser_star)
                | Op::postfix(Rule::parser_plus)
                | Op::postfix(Rule::parser_optional))
    };
}

fn unescape_qouted(string: &str) -> Option<String> {
    unescape(&string[1..string.len() - 1])
}

// from pest
fn unescape(string: &str) -> Option<String> {
    let mut result = String::new();
    let mut chars = string.chars();

    loop {
        match chars.next() {
            Some('\\') => match chars.next()? {
                '"' => result.push('"'),
                '\\' => result.push('\\'),
                'r' => result.push('\r'),
                'n' => result.push('\n'),
                't' => result.push('\t'),
                '0' => result.push('\0'),
                '\'' => result.push('\''),
                'x' => {
                    let string: String = chars.clone().take(2).collect();

                    if string.len() != 2 {
                        return None;
                    }

                    for _ in 0..string.len() {
                        chars.next()?;
                    }

                    let value = u8::from_str_radix(&string, 16).ok()?;

                    result.push(char::from(value));
                }
                'u' => {
                    if chars.next()? != '{' {
                        return None;
                    }

                    let string: String = chars.clone().take_while(|c| *c != '}').collect();

                    if string.len() < 2 || 6 < string.len() {
                        return None;
                    }

                    for _ in 0..string.len() + 1 {
                        chars.next()?;
                    }

                    let value = u32::from_str_radix(&string, 16).ok()?;

                    result.push(char::from_u32(value)?);
                }
                _ => return None,
            },
            Some(c) => result.push(c),
            None => return Some(result),
        };
    }
}

#[derive(Debug, Clone)]
pub struct WithSpan<'a, T> {
    pub span: Span<'a>,
    pub node: T,
}

impl<'a, T: Display> Display for WithSpan<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}
pub type SpanBox<'a, T> = Box<WithSpan<'a, T>>;

#[derive(Debug)]
pub enum SurfaceSyntaxTree<'a> {
    Grammar {
        lexer: SpanBox<'a, Self>,
        parser: SpanBox<'a, Self>,
    },
    Parser {
        entrypoint: WithSpan<'a, ()>,
        rules: Vec<WithSpan<'a, Self>>,
    },
    Lexer {
        rules: Vec<WithSpan<'a, Self>>,
    },
    LexicalAlternative {
        lhs: SpanBox<'a, Self>,
        rhs: SpanBox<'a, Self>,
    },
    LexicalSequence {
        lhs: SpanBox<'a, Self>,
        rhs: SpanBox<'a, Self>,
    },
    LexicalStar {
        inner: SpanBox<'a, Self>,
    },
    LexicalPlus {
        inner: SpanBox<'a, Self>,
    },
    LexicalOptional {
        inner: SpanBox<'a, Self>,
    },
    LexicalNot {
        inner: SpanBox<'a, Self>,
    },
    ParserAlternative {
        lhs: SpanBox<'a, Self>,
        rhs: SpanBox<'a, Self>,
    },
    ParserSequence {
        lhs: SpanBox<'a, Self>,
        rhs: SpanBox<'a, Self>,
    },
    ParserStar {
        inner: SpanBox<'a, Self>,
    },
    ParserPlus {
        inner: SpanBox<'a, Self>,
    },
    ParserOptional {
        inner: SpanBox<'a, Self>,
    },
    LexicalDefinition {
        name: WithSpan<'a, ()>,
        expr: SpanBox<'a, Self>,
    },
    LexicalToken {
        active: bool,
        name: WithSpan<'a, ()>,
        expr: SpanBox<'a, Self>,
    },
    Range {
        start: char,
        end: char,
    },
    String(String),
    Bottom,
    Empty,
    Char {
        value: WithSpan<'a, char>,
    },
    ParserDefinition {
        active: bool,
        name: WithSpan<'a, ()>,
        expr: SpanBox<'a, Self>,
    },
    ParserFixpoint {
        active: bool,
        name: WithSpan<'a, ()>,
        expr: SpanBox<'a, Self>,
    },
    ParserRuleRef {
        name: WithSpan<'a, ()>,
    },
    LexicalRuleRef {
        name: WithSpan<'a, ()>,
    },
}

fn parse_surface_syntax<'a, I: Iterator<Item = Pair<'a, Rule>>>(
    pairs: I,
    pratt: &PrattParser<Rule>,
    src: &'a str,
) -> Result<WithSpan<'a, SurfaceSyntaxTree<'a>>, GrammarDefinitionError<'a>> {
    pratt
        .map_primary(|primary| {
            let span = primary.as_span();
            match primary.as_rule() {
                Rule::grammar => {
                    let mut grammar = primary.into_inner();
                    let lexer = grammar.next().ok_or_else(|| unexpected_eoi!("lexer"))?;
                    let parser = grammar.next().ok_or_else(|| unexpected_eoi!("parser"))?;
                    let lexer = parse_surface_syntax([lexer].into_iter(), pratt, src)?;
                    let parser = parse_surface_syntax([parser].into_iter(), pratt, src)?;
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::Grammar {
                            lexer: Box::new(lexer),
                            parser: Box::new(parser),
                        },
                    })
                }
                Rule::lexer_def => {
                    let lexer_rules = primary
                        .into_inner()
                        .next()
                        .ok_or_else(|| unexpected_eoi!("lexer rules"))?;
                    let rules = lexer_rules.into_inner().fold(Ok(Vec::new()), |acc, rule| {
                        acc.and_then(|vec| {
                            parse_surface_syntax([rule].into_iter(), pratt, src).map(|rule| {
                                let mut vec = vec;
                                vec.push(rule);
                                vec
                            })
                        })
                    })?;
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::Lexer { rules },
                    })
                }
                Rule::lexical_definition => {
                    let mut definition = primary.into_inner();
                    let name = definition
                        .next()
                        .ok_or_else(|| unexpected_eoi!("name for lexical definition"))?;
                    let expr = definition
                        .next()
                        .ok_or_else(|| unexpected_eoi!("expr for lexical definition"))?;
                    let name = WithSpan {
                        span: name.as_span(),
                        node: (),
                    };
                    let expr = parse_surface_syntax(expr.into_inner(), pratt, src)?;
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::LexicalDefinition {
                            name,
                            expr: Box::new(expr),
                        },
                    })
                }
                Rule::range => {
                    let mut primary = primary.into_inner();
                    let start = primary
                        .next()
                        .ok_or_else(|| unexpected_eoi!("start character for range"))?;
                    let end = primary
                        .next()
                        .ok_or_else(|| unexpected_eoi!("end character for range"))?;
                    let start = unescape_qouted(start.as_str())
                        .ok_or_else(|| format_error!(span, "failed to unescape character"))?
                        .parse()
                        .map_err(|e| format_error!(span, "{}", e))?;
                    let end = unescape_qouted(end.as_str())
                        .ok_or_else(|| format_error!(span, "failed to unescape character"))?
                        .parse()
                        .map_err(|e| format_error!(span, "{}", e))?;
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::Range { start, end },
                    })
                }
                Rule::string => {
                    let value = unescape_qouted(primary.as_str())
                        .ok_or_else(|| format_error!(span, "failed to unescape string"))?;
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::String(value),
                    })
                }
                Rule::lexical_expr => parse_surface_syntax(primary.into_inner(), pratt, src),
                Rule::active_token | Rule::silent_token => {
                    let active = matches!(primary.as_rule(), Rule::active_token);
                    let mut token = primary.into_inner();
                    let name = token
                        .next()
                        .ok_or_else(|| unexpected_eoi!("name for token rule"))?;
                    let expr = token
                        .next()
                        .ok_or_else(|| unexpected_eoi!("expr for token rule"))?;
                    let name = WithSpan {
                        span: name.as_span(),
                        node: (),
                    };
                    let expr = parse_surface_syntax(expr.into_inner(), pratt, src)?;
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::LexicalToken {
                            active,
                            name,
                            expr: Box::new(expr),
                        },
                    })
                }
                Rule::character => {
                    let character = unescape_qouted(primary.as_str())
                        .ok_or_else(|| format_error!(span, "failed to unescape character"))?
                        .parse()
                        .map_err(|e| format_error!(span, "{}", e))?;
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::Char {
                            value: WithSpan {
                                span,
                                node: character,
                            },
                        },
                    })
                }
                Rule::token_id => {
                    // token ref
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::LexicalRuleRef {
                            name: WithSpan { span, node: () },
                        },
                    })
                }
                Rule::parser_id => {
                    // token ref
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::ParserRuleRef {
                            name: WithSpan { span, node: () },
                        },
                    })
                }
                Rule::bottom => {
                    // token ref
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::Bottom,
                    })
                }
                Rule::empty => {
                    // token ref
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::Empty,
                    })
                }
                Rule::parser_def => {
                    let mut parser_rules = primary.into_inner();
                    let entrypoint = parser_rules
                        .next()
                        .ok_or_else(|| unexpected_eoi!("entrypoint for parser"))?;
                    let entrypoint = WithSpan {
                        span: entrypoint.as_span(),
                        node: (),
                    };
                    let parser_rules = parser_rules
                        .next()
                        .ok_or_else(|| unexpected_eoi!("parser rules"))?;
                    let rules = parser_rules
                        .into_inner()
                        .fold(Ok(Vec::new()), |acc, rule| {
                            acc.and_then(|vec| {
                                parse_surface_syntax([rule].into_iter(), pratt, src).map(|rule| {
                                    let mut vec = vec;
                                    vec.push(rule);
                                    vec
                                })
                            })
                        })?;
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::Parser { entrypoint, rules },
                    })
                }
                Rule::active_parser_definition | Rule::silent_parser_definition => {
                    let active = matches!(primary.as_rule(), Rule::active_parser_definition);
                    let mut definition = primary.into_inner();
                    let name = definition
                        .next()
                        .ok_or_else(|| unexpected_eoi!("name for token rule"))?;
                    let expr = definition
                        .next()
                        .ok_or_else(|| unexpected_eoi!("expr for token rule"))?;
                    let name = WithSpan {
                        span: name.as_span(),
                        node: (),
                    };
                    let expr = parse_surface_syntax(expr.into_inner(), pratt, src)?;
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::ParserDefinition {
                            active,
                            name,
                            expr: Box::new(expr),
                        },
                    })
                }
                Rule::active_parser_fixpoint | Rule::silent_parser_fixpoint => {
                    let active = matches!(primary.as_rule(), Rule::active_parser_fixpoint);
                    let mut fixpoint = primary.into_inner();
                    let name = fixpoint
                        .next()
                        .ok_or_else(|| unexpected_eoi!("name for token rule"))?;
                    let expr = fixpoint
                        .next()
                        .ok_or_else(|| unexpected_eoi!("expr for token rule"))?;
                    let name = WithSpan {
                        span: name.as_span(),
                        node: (),
                    };
                    let expr = parse_surface_syntax(expr.into_inner(), pratt, src)?;
                    Ok(WithSpan {
                        span,
                        node: SurfaceSyntaxTree::ParserFixpoint {
                            active,
                            name,
                            expr: Box::new(expr),
                        },
                    })
                }
                Rule::parser_expr => parse_surface_syntax(primary.into_inner(), pratt, src),
                _ => {
                    unreachable_branch!("undefined primary rule: {:?}", primary.as_rule())
                }
            }
        })
        .map_prefix(|op, operand| {
            let operand = operand?;
            match op.as_rule() {
                Rule::lexical_not => {
                    let total_span = Span::new(src, op.as_span().start(), operand.span.end())
                        .ok_or_else(|| parser_logical_error!("invalid span"))?;
                    Ok(WithSpan {
                        span: total_span,
                        node: SurfaceSyntaxTree::LexicalNot {
                            inner: Box::new(operand),
                        },
                    })
                }
                _ => unreachable_branch!("only lexical not is supported as a prefix operator"),
            }
        })
        .map_infix(|lhs, op, rhs| {
            let lhs = lhs?;
            let rhs = rhs?;
            let total_span = Span::new(src, lhs.span.start(), rhs.span.end())
                .ok_or_else(|| parser_logical_error!("invalid span"))?;
            match op.as_rule() {
                Rule::lexical_alternative => Ok(WithSpan {
                    span: total_span,
                    node: SurfaceSyntaxTree::LexicalAlternative {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                }),
                Rule::lexical_sequence => Ok(WithSpan {
                    span: total_span,
                    node: SurfaceSyntaxTree::LexicalSequence {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                }),
                Rule::parser_alternative => Ok(WithSpan {
                    span: total_span,
                    node: SurfaceSyntaxTree::ParserAlternative {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                }),
                Rule::parser_sequence => Ok(WithSpan {
                    span: total_span,
                    node: SurfaceSyntaxTree::ParserSequence {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                }),

                _ => unreachable_branch!("Operator {} is not an infix operator", op.as_str()),
            }
        })
        .map_postfix(|expr, op| {
            let expr = expr?;
            let op_span = op.as_span();
            let total_span = Span::new(src, expr.span.start(), op_span.end())
                .ok_or_else(|| unexpected_eoi!("invalid span"))?;
            match op.as_rule() {
                Rule::lexical_plus => Ok(WithSpan {
                    span: total_span,
                    node: SurfaceSyntaxTree::LexicalPlus {
                        inner: Box::new(expr),
                    },
                }),
                Rule::lexical_star => Ok(WithSpan {
                    span: total_span,
                    node: SurfaceSyntaxTree::LexicalStar {
                        inner: Box::new(expr),
                    },
                }),
                Rule::lexical_optional => Ok(WithSpan {
                    span: total_span,
                    node: SurfaceSyntaxTree::LexicalOptional {
                        inner: Box::new(expr),
                    },
                }),
                Rule::parser_plus => Ok(WithSpan {
                    span: total_span,
                    node: SurfaceSyntaxTree::ParserPlus {
                        inner: Box::new(expr),
                    },
                }),
                Rule::parser_star => Ok(WithSpan {
                    span: total_span,
                    node: SurfaceSyntaxTree::ParserStar {
                        inner: Box::new(expr),
                    },
                }),
                Rule::parser_optional => Ok(WithSpan {
                    span: total_span,
                    node: SurfaceSyntaxTree::ParserOptional {
                        inner: Box::new(expr),
                    },
                }),
                _ => unreachable_branch!("Operator {} is not a postfix operator", op.as_str()),
            }
        })
        .parse(pairs)
}

pub fn parse(input: &str) -> Result<WithSpan<SurfaceSyntaxTree>, crate::Error> {
    match <GrammarParser as pest::Parser<Rule>>::parse(Rule::grammar, input) {
        Ok(pairs) => parse_surface_syntax(pairs, &PRATT_PARSER, input)
            .map_err(crate::Error::GrammarDefinitionError),
        Err(e) => Err(crate::Error::GrammarDefinitionError(
            GrammarDefinitionError::SyntaxError(Box::new(e)),
        )),
    }
}

#[cfg(test)]
mod test {
    use std::mem::size_of;

    use ariadne::Source;
    use pest::Parser;
    use typed_arena::Arena;

    use crate::{
        core_syntax::TermArena,
        frontend::lexical::LexerDatabase,
        fusion::fusion_parser,
        nf::{
            fully_normalize, merge_inactive_rules, remove_unreachable_rules, semi_normalize,
            NormalForm, NormalForms, Tag, TagAssigner,
        },
    };

    use super::{syntax::construct_parser, *};

    const TEST: &str = include_str!("example.pag");

    #[test]
    fn it_parses_lexical_expr() {
        println!("{}", size_of::<NormalForm>());
        let pairs = GrammarParser::parse(Rule::grammar, TEST).unwrap();
        let tree = parse_surface_syntax(pairs, &PRATT_PARSER, TEST).unwrap();
        let SurfaceSyntaxTree::Grammar { lexer, parser } = &tree.node else { unreachable!() };

        let database = LexerDatabase::new(lexer).unwrap();
        for (i, rule) in database.entries.iter() {
            println!("{i} ::= {}, active = {}", rule.rule, rule.active)
        }
        println!("----");
        let arena = TermArena::new();
        let parser = construct_parser(&arena, database, parser).unwrap();
        for (i, rule) in parser.bindings.iter() {
            println!("{i} ::= {}, active = {}", rule.term, rule.active)
        }
        let errs = parser.type_check();

        let liberr = crate::Error::from(errs);
        let reports = liberr.to_reports("example.pag");
        let mut src = ("example.pag", Source::from(TEST));
        for i in reports.iter() {
            i.eprint(&mut src).unwrap();
        }
        assert!(reports.is_empty());
        println!("----");
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
        println!("size of nfs.entries: {}", nfs.entries.len());
        println!("{}", nfs);
        println!("----");
        fully_normalize(&nf_arena, &mut nfs);
        println!("size of nfs.entries: {}", nfs.entries.len());
        println!("{}", nfs);
        println!("----");
        merge_inactive_rules(&mut nfs, &parser, &nf_arena);
        println!("size of nfs.entries: {}", nfs.entries.len());
        println!("{}", nfs);
        println!("----");
        remove_unreachable_rules(&mut nfs, &parser);
        println!("size of nfs.entries: {}", nfs.entries.len());
        println!("{}", nfs);
        println!("----");
        let parser = fusion_parser(&nfs, &parser);
        println!("{}", parser);
        //println!("{:#?}", tree)
    }
}
