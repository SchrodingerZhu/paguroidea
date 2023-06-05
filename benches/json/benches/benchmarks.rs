use criterion::{criterion_group, criterion_main, Criterion};
use pag_json::{generate_random_json, parse};
use pest::Parser;
use pest_json::{JSONParser, Rule};
use serde_json::Value;

mod lalrlexer;
pub use lalrlexer::Pvalue;

use logos::Logos;
use std::fmt;
use std::ops::Range;

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \r\n\t]+")]
pub enum Token<'a> {
    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("null")]
    Null,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LBrack,

    #[token("]")]
    RBrack,

    #[regex(r#"-?([0-9]|([1-9][0-9]*))((\.[0-9]+)?)([eE][+-]?[0-9]+)?"#, |lex| lex.slice())]
    Number(&'a str),
    #[regex(r#""([ -!#-\[\]-\x{10ffff}]|([\\](["\\/bfnrt]|[u][[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]])))*""#)]
    String(&'a str),
    //#[regex(r#""([ -!#-\[\]-\x{10ffff}]|([\\](["\\/bfnrt]|[u][[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]])))*"#)]
    //MissingEndQuote(&'a str),
}

impl<'a> Token<'a> {
    pub fn to_triple(
        (t, r): (Result<Token<'a>, ()>, Range<usize>),
    ) -> Result<(usize, Token, usize), &'static str> {
        Ok((r.start, t.unwrap(), r.end))
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(json, "/benches/json.rs"); // synthesized by LALRPOP

mod pest_json {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "benches/json.pest"]
    pub struct JSONParser;
}

#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

fn criterion_benchmark(c: &mut Criterion) {
    let mut g = c.benchmark_group("random-json");
    let data = generate_random_json(10);
    g.throughput(criterion::Throughput::Bytes(data.bytes().len() as u64));
    g.bench_function("pag-json", |b| {
        b.iter(|| {
            parse(&data).unwrap();
        })
    });
    g.bench_function("serde-json", |b| {
        b.iter(|| {
            serde_json::from_str::<Value>(&data).unwrap();
        })
    });
    g.bench_function("pest-json", |b| {
        b.iter(|| {
            JSONParser::parse(Rule::json, &data).unwrap();
        })
    });
    g.bench_function("lalrpop-json", |b| {
        b.iter(|| {
            let lexer = Token::lexer(&data).spanned().map(Token::to_triple);
            json::JsonParser::new().parse(lexer).unwrap();
        })
    });

    g.finish();
    let mut g = c.benchmark_group("twitter-json");
    let data = include_str!("twitter.json");
    g.throughput(criterion::Throughput::Bytes(data.bytes().len() as u64));
    g.bench_function("pag-json", |b| {
        b.iter(|| {
            parse(data).unwrap();
        })
    });
    g.bench_function("serde-json", |b| {
        b.iter(|| {
            serde_json::from_str::<Value>(data).unwrap();
        })
    });
    g.bench_function("pest-json", |b| {
        b.iter(|| {
            JSONParser::parse(Rule::json, data).unwrap();
        })
    });
    g.bench_function("lalrpop-json", |b| {
        b.iter(|| {
            let lexer = Token::lexer(data).spanned().map(Token::to_triple);
            json::JsonParser::new().parse(lexer).unwrap();
        })
    });
    g.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
