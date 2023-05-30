use std::num::Wrapping;

mod parser;

#[allow(dead_code)]
fn eval(tree: &parser::ParserTree) -> Wrapping<usize> {
    match tree.tag() {
        parser::Tag::expr => tree.children()[..].iter().map(eval).sum(),
        parser::Tag::mult => tree.children()[..].iter().map(eval).product(),
        parser::Tag::int => Wrapping(tree.as_slice().parse::<usize>().unwrap()),
        parser::Tag::special => {
            assert_eq!(tree.as_slice().chars().count(), 1);
            Wrapping(tree.as_slice().chars().nth(0).unwrap() as usize)
        }
    }
}

#[allow(dead_code)]
fn generate_random_expr<G: rand::Rng>(rng: &mut G, depth: usize) -> (Wrapping<usize>, String) {
    if depth == 0 {
        let x = rng.gen_range(0..100);
        return (Wrapping(x), format!("{}", x));
    }
    match rng.gen_range(0..4) {
        0 => {
            let x = rng.gen_range(0..100);
            (Wrapping(x), format!("{}", x))
        }
        1 => {
            let x = rng.gen_range(0xFF..=0xD7FF);
            (
                Wrapping(x),
                format!("{}", char::from_u32(x as u32).unwrap()),
            )
        }
        2 => {
            let (a, s1) = generate_random_expr(rng, depth - 1);
            let (b, s2) = generate_random_expr(rng, depth - 1);
            (a + b, format!("({} + {})", s1, s2))
        }
        _ => {
            let (a, s1) = generate_random_expr(rng, depth - 1);
            let (b, s2) = generate_random_expr(rng, depth - 1);
            (a * b, format!("({} * {})", s1, s2))
        }
    }
}

#[test]
fn simple_test() {
    let expr = "55 * (14 + 15) + 66 * 13";
    let tree = parser::parse(expr).unwrap();
    assert_eq!(eval(&tree), Wrapping(55 * (14 + 15) + 66 * 13));
    // (8 * 1 + 3) * 6 + ((37 + 7) * 2)
    let expr = "(8 * 1 + 3) * 6 + ((37 + 7) * 2)";
    let tree = parser::parse(expr).unwrap();
    assert_eq!(eval(&tree), Wrapping((8 + 3) * 6 + ((37 + 7) * 2)));

    // ((((8 + 13) + 3) * 6) * ((3 + 7) * 22)) * 91
    let expr = "((((8 + 13) + 3) * 6) * ((3 + 7) * 22)) * 91 + 1 + 2 + 3";
    let tree = parser::parse(expr).unwrap();
    assert_eq!(
        eval(&tree),
        Wrapping(((((8 + 13) + 3) * 6) * ((3 + 7) * 22)) * 91 + 1 + 2 + 3)
    );
}
#[test]
fn random_test() {
    for _ in 0..1000 {
        let (value, expr) = generate_random_expr(&mut rand::thread_rng(), 15);
        let tree = parser::parse(&expr).unwrap();
        assert_eq!(eval(&tree), value);
    }
}
