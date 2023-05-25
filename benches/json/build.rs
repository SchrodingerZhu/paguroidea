fn main() {
    pag_compiler::compile("json.pag", "src/parser.rs");
    println!("cargo:rerun-if-changed=json.pag");
}
