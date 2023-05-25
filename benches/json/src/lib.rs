mod parser;

pub use parser::parse;

#[test]
fn test_json() {
    let json = r#"{ "hello": { "values": [{}, [], [1, 1e3, -0.5, 9.99]] }, "age" : 13 }"#;
    let tree = parser::parse(json).unwrap();
    println!("{:#?}", tree);
}
