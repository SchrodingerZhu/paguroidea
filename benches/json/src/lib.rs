#![feature(portable_simd)]
#![feature(core_intrinsics)]
#![feature(array_chunks)]
mod parser;

pub use parser::parse;
use rand::Rng;
use serde_json::Value;

fn generate_json_value<G: Rng>(depth: usize, gen: &mut G) -> Value {
    if depth == 0 {
        match gen.gen_range(0..4) {
            0 => Value::Null,
            1 => Value::Bool(gen.gen()),
            2 => Value::Number(serde_json::Number::from_f64(gen.gen()).unwrap()),
            _ => Value::String(gen.gen::<u64>().to_string()),
        }
    } else {
        match gen.gen_range(0..7) {
            0 => Value::Null,
            1 => Value::Bool(gen.gen()),
            2 => Value::String(gen.gen::<u64>().to_string()),
            3 | 4 => {
                let mut array = Vec::new();
                for _ in 0..gen.gen_range(0..10) {
                    array.push(generate_json_value(depth - 1, gen));
                }
                Value::Array(array)
            }
            _ => {
                let mut object = serde_json::Map::new();
                for _ in 0..gen.gen_range(0..10) {
                    object.insert(
                        gen.gen::<u64>().to_string(),
                        generate_json_value(depth - 1, gen),
                    );
                }
                Value::Object(object)
            }
        }
    }
}

pub fn generate_random_json(depth: usize) -> String {
    let mut random = rand::thread_rng();
    let mut buffer = Vec::new();
    let value = generate_json_value(depth, &mut random);
    serde_json::to_writer(&mut buffer, &value).unwrap();
    unsafe { String::from_utf8_unchecked(buffer) }
}

#[test]
fn test_json() {
    let json = r#"{ "hello": { "values": [{}, [], [1, 1e3, -0.5, 9.99]] }, "age" : 13 }"#;
    let tree = parser::parse(json).unwrap();
    println!("{:#?}", tree);
}
#[test]
fn test_random() {
    for _ in 0..10 {
        let json = generate_random_json(10);
        let parsed = parser::parse(&json).unwrap();
        assert_eq!(json.len(), parsed.len())
    }
}

#[test]
fn test_twitter() {
    let json = include_str!("../benches/twitter.json");
    let parsed = parser::parse(json).unwrap();
    assert_eq!(json.len(), parsed.len())
}
