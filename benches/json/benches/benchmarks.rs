use criterion::{criterion_group, criterion_main, Criterion};
use lalrpop_util::lalrpop_mod;
use pag_json::{generate_random_json, parse};
use pest::Parser;
use pest_json::Rule;
use serde_json::Value;

mod lalr_def;
pub use lalr_def::{Pvalue, Token};

#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

lalrpop_mod!(lalrpop_json, "/benches/json.rs");
lalrpop_mod!(lalrpop_logos_json, "/benches/json_logos.rs");

mod pest_json {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "benches/json.pest"]
    pub struct JSONParser;
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut g = c.benchmark_group("random-json");
    let data = generate_random_json(10);
    g.throughput(criterion::Throughput::Bytes(data.bytes().len() as u64));
    g.bench_function("pag", |b| {
        b.iter(|| {
            parse(&data).unwrap();
        })
    });
    g.bench_function("serde", |b| {
        b.iter(|| {
            serde_json::from_str::<Value>(&data).unwrap();
        })
    });
    g.bench_function("pest", |b| {
        b.iter(|| {
            pest_json::JSONParser::parse(Rule::json, &data).unwrap();
        })
    });
    g.bench_function("lalrpop", |b| {
        b.iter(|| {
            lalrpop_json::JsonParser::new().parse(&data).unwrap();
        })
    });
    g.bench_function("lalrpop+logos", |b| {
        b.iter(|| {
            let lexer = Token::lalrpop_lexer(&data);
            lalrpop_logos_json::JsonParser::new().parse(lexer).unwrap();
        })
    });
    g.finish();

    let mut g = c.benchmark_group("twitter-json");
    let data = include_str!("twitter.json");
    g.throughput(criterion::Throughput::Bytes(data.bytes().len() as u64));
    g.bench_function("pag", |b| {
        b.iter(|| {
            parse(data).unwrap();
        })
    });
    g.bench_function("serde", |b| {
        b.iter(|| {
            serde_json::from_str::<Value>(data).unwrap();
        })
    });
    g.bench_function("pest", |b| {
        b.iter(|| {
            pest_json::JSONParser::parse(Rule::json, data).unwrap();
        })
    });
    g.bench_function("lalrpop", |b| {
        b.iter(|| {
            lalrpop_json::JsonParser::new().parse(&data).unwrap();
        })
    });
    g.bench_function("lalrpop+logos", |b| {
        b.iter(|| {
            let lexer = Token::lalrpop_lexer(&data);
            lalrpop_logos_json::JsonParser::new().parse(lexer).unwrap();
        })
    });
    g.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
