mod parser;

pub use parser::parse;
use rand::Rng;

pub fn generate_csv(line: usize, width: usize) -> String {
    let mut random = rand::thread_rng();
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
    parser::parse(&generate_csv(500, 500)).unwrap();
}
