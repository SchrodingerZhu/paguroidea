fn main() {
    pag_compiler::compile("arith.pag", "src/parser.rs");
    println!("cargo:rerun-if-changed=arith.pag");
}
