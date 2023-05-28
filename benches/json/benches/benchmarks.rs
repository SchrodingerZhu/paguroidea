use criterion::{criterion_group, criterion_main, Criterion};
use pag_json::{generate_random_json, parse};
use pest::Parser;
use pest_json::{JSONParser, Rule};
use serde_json::Value;

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
    g.finish();
    let mut g = c.benchmark_group("twitter-json");
    let data = include_str!("twitter.json");
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
    g.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
