#![feature(portable_simd)]
#![feature(core_intrinsics)]
#![feature(array_chunks)]
mod parser;

pub use parser::parse;
use rand::prelude::StdRng;
use rand::{Rng, SeedableRng};

pub fn generate_csv(line: usize, width: usize) -> String {
    let mut random = std::env::var("PAG_RANDOM_SEED")
        .ok()
        .and_then(|x| x.parse().ok())
        .map_or_else(StdRng::from_entropy, StdRng::seed_from_u64);
    let mut buffer = String::new();
    for _ in 0..line {
        for i in 0..width {
            if random.gen::<f64>() < 0.5 {
                buffer.push_str(&format!("\"{}\"", random.gen::<f64>()));
            } else {
                buffer.push_str(&format!("{}", random.gen::<u64>()));
            }

            if i != width - 1 {
                buffer.push(',');
            }
        }
        buffer.push_str("\r\n");
    }
    buffer
}

#[test]
fn test_csv() {
    let data = generate_csv(500, 500);
    let parsed = parser::parse(&data).unwrap();
    assert_eq!(parsed.len(), data.len());
}
