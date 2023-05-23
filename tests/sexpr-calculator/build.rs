
fn main() {
    pag_compiler::compile(
        "sexpr.pag",
        "src/parser.rs",
    );
    println!("cargo:rerun-if-changed=sexpr.pag");
}