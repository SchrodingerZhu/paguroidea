use std::{path::PathBuf, todo};

use quote::quote;

use proc_macro2::{Literal, TokenStream};
use syn::spanned::Spanned;

fn derive_parser(input: TokenStream) -> Result<TokenStream, syn::Error> {
    let input: syn::DeriveInput = syn::parse2(input)?;
    match input.attrs.iter().find(|x| x.path().is_ident("grammar")) {
        Some(attr) => {
            let literal: Literal = attr.parse_args()?;
            let root = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
            let path = PathBuf::from(root).join(literal.to_string().trim_matches('"'));
            match std::fs::read_to_string(path) {
                Ok(input) => {
                    todo!()
                }
                Err(e) => Err(syn::Error::new(literal.span(), e)),
            }
        }
        None => Err(syn::Error::new(input.span(), "missing grammar attribute")),
    }
}

#[test]
fn test() {
    let stream = quote! {
        #[derive(Parser)]
        #[grammar("grammar")]
        struct Parser;
    };
    derive_parser(stream.into());
}
