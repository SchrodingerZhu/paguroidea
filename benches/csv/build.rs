fn main() {
    pag_compiler::compile("csv.pag", "src/parser.rs");
    println!("cargo:rerun-if-changed=csv.pag");
}
