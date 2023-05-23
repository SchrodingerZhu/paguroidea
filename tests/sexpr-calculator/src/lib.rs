#![allow(dead_code)]
mod parser;

fn eval(tree: &parser::ParserTree) -> usize {
    match tree.tag() {
        parser::Tag::sexpr => {
            eval(&tree.children()[0])
        },
        parser::Tag::int => {
            tree.as_slice().parse().unwrap()
        },
        parser::Tag::op => {
            unreachable!("op should be handled by sexpr")
        },
        parser::Tag::compound => {
            match tree.children()[0].as_slice() {
                "+" => {
                    tree.children()[1..].iter().map(|x| eval(x)).sum()
                },
                "*" => {
                    tree.children()[1..].iter().map(|x| eval(x)).product()
                },
                _ => unreachable!("only + and * are supported")
            }
        },
        
    }
}

#[test]
fn simple_test() {
    let test = "(+ 1 (* 5 55))";
    let tree = parser::parse(test).unwrap();
    assert_eq!(276, eval(&tree));
    let test = "(+ 1 (# 5 5))";
    let err = parser::parse(test).unwrap_err().to_string();
    assert_eq!(err, "expecting MULT, PLUS or WHITESPACE for compound at offset 6");
}
