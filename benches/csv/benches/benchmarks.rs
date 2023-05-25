use criterion::{criterion_group, criterion_main, Criterion};
use csv::StringRecord;
use pag_csv::{generate_csv, parse};

mod pest_csv {
    use pest_derive::Parser;

    #[derive(Parser)]
    #[grammar = "benches/csv.pest"]
    pub struct CSVParser;
}

#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

fn csv_read_all(input: &str) -> Vec<StringRecord> {
    let mut records = Vec::new();
    csv::Reader::from_reader(input.as_bytes())
        .into_records()
        .for_each(|r| records.push(r.unwrap()));
    records
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut g = c.benchmark_group("throughput");
    let data = generate_csv(1000, 20);
    g.throughput(criterion::Throughput::Bytes(data.bytes().len() as u64));
    g.bench_function("pag", |b| {
        b.iter(|| {
            assert_eq!(parse(&data).unwrap().children().len(), 1000);
        })
    });
    g.bench_function("csv", |b| {
        b.iter(|| {
            assert_eq!(csv_read_all(&data).len(), 999);
        })
    });
    g.bench_function("pest", |b| {
        b.iter(|| {
            use pest::Parser;
            let pairs = pest_csv::CSVParser::parse(pest_csv::Rule::csv, &data).unwrap();
            assert_eq!(pairs.into_iter().next().unwrap().into_inner().len(), 1000);
        })
    });
    g.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
