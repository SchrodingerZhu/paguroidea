#![feature(portable_simd)]
#![feature(core_intrinsics)]
#![feature(array_chunks)]
use std::num::Wrapping;

mod parser;

#[allow(dead_code)]
fn eval(tree: &parser::ParseTree) -> Wrapping<usize> {
    match tree.tag() {
        parser::Tag::sexpr => eval(&tree.children()[0]),
        parser::Tag::int => Wrapping(tree.as_slice().parse::<usize>().unwrap()),
        parser::Tag::op => {
            unreachable!("op should be handled by sexpr")
        }
        parser::Tag::compound => match tree.children()[0].as_slice() {
            "+" | "加" => tree.children()[1..].iter().map(eval).sum(),
            "*" | "乘" => tree.children()[1..].iter().map(eval).product(),
            other => unreachable!("only '+' and '*' are supported, found '{other}'"),
        },
    }
}

#[allow(dead_code)]
fn generate_sexpr<G: rand::Rng>(mut limit: usize, gen: &mut G) -> (usize, Wrapping<usize>, String) {
    if limit <= 1 {
        let x = Wrapping(gen.next_u64() as usize % 100);
        return (1, x, format!("{}", x));
    }
    match gen.next_u64() % 20 {
        0 => {
            let x = Wrapping(gen.next_u64() as usize % 100);
            (1, x, format!("{}", x))
        }
        1..=15 => {
            let width = 2 + gen.next_u64() % (limit as u64).min(10);
            let mut buffer = if gen.gen_bool(0.5) {
                "(+".to_string()
            } else {
                "(加".to_string()
            };
            let mut cnt = 0;
            let mut sum = Wrapping(0);
            for _ in 0..width {
                let (w, v, s) = generate_sexpr(limit, gen);
                cnt += w;
                limit = limit.saturating_sub(w);
                sum += v;
                buffer.push_str(&format!(" {}", s));
            }
            buffer.push(')');
            (cnt, sum, buffer)
        }
        _ => {
            let width = 2 + gen.next_u64() % (limit as u64).min(10);
            let mut buffer = if gen.gen_bool(0.5) {
                "(*".to_string()
            } else {
                "(乘".to_string()
            };

            let mut cnt = 0;
            let mut prod = Wrapping(1);
            for _ in 0..width {
                let (w, v, s) = generate_sexpr(limit, gen);
                cnt += w;
                limit = limit.saturating_sub(w);
                prod *= v;
                buffer.push_str(&format!(" {}", s));
            }
            buffer.push(')');
            (cnt, prod, buffer)
        }
    }
}

#[test]
fn simple_test() {
    let test = "(加 1 (* 5 55))";
    let tree = parser::parse(test).unwrap();
    assert_eq!(276, eval(&tree).0);
    let test = "(+ 1 (# 5 5))";
    let err = parser::parse(test).unwrap_err().to_string();
    assert_eq!(err, "expecting MULT or PLUS for compound at offset 6");
}

#[test]
fn randomized_test() {
    for _ in 0..1000 {
        let (_, value, expr) = generate_sexpr(20, &mut rand::thread_rng());

        let tree = parser::parse(&expr).unwrap();
        assert_eq!(value, eval(&tree))
    }
}
