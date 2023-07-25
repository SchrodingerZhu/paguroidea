use std::collections::HashMap;

use quote::format_ident;
use syn::{parse_quote, Expr, ExprCall, Stmt, Type};

use super::Tag;

pub type SemActTable = HashMap<Tag, SemAct>;
pub struct SemAct {
    /// Identifier of the semantic action routine.
    function: Expr,
    /// Type annotation
    ty: Option<Type>,
    /// Number of arguments
    arity: usize,
}

impl SemAct {
    fn generate_call(&self) -> ExprCall {
        let exprs = (0..self.arity).map(|i| format_ident!("__{}", i));
        let function = &self.function;
        parse_quote!(
            #function(#(#exprs),*)
        )
    }
    pub fn generate_statement(&self, output: Option<usize>) -> Stmt {
        let expr = self.generate_call();
        match output {
            None => parse_quote!(
                #expr;
            ),
            Some(index) => {
                let ty = self.ty.iter();
                let output = format_ident!("__{}", index);
                parse_quote!(
                    let #output #(: #ty)* = #expr;
                )
            }
        }
    }
}
