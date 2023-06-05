extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new().process_dir("benches/").unwrap();
    pag_compiler::compile("json.pag", "src/parser.rs");
    println!("cargo:rerun-if-changed=json.pag");
}
