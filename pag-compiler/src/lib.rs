use std::path::Path;

use syn::File;

// to be used in build.rs
pub fn compile<I: AsRef<Path>, O: AsRef<Path>>(input: I, output: O) {
    let data = std::fs::read_to_string(input.as_ref()).unwrap();
    match pag_parser::generate_parser(&data) {
        Ok(tokens) => {
            let tree: File = syn::parse2(tokens).unwrap();
            let prettified = prettyplease::unparse(&tree);
            std::fs::write(output.as_ref(), prettified).unwrap();
        }
        Err(errs) => {
            errs.report_stderr(&format!("{}", input.as_ref().display()), &data)
                .unwrap();
            panic!("failed to compile parser")
        }
    }
}
