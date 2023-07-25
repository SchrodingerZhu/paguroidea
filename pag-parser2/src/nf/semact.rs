// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;

use super::Tag;
use syn::{parse_quote, Expr, Type};

pub type SemActTable = HashMap<Tag, SemAct>;

///
/// ```
/// trait Collector<T> {
///     pub type Output;
///     fn finalize(self) -> Self::Output;
///     fn collect(&mut self, data: T);
/// }
///
/// ```
pub enum SemAct {
    CustomizedRoutine {
        /// Identifier of the semantic action routine.
        function: Expr,
        /// Type annotation
        ret_type: Type,
        /// Number of arguments
        arity: usize,
    },
    /// Specialized for the inner of @(@a, @b, @c). Return an Tuple of the inner routine.
    Tuple,
    /// Specialized for `inner?`. Return an Option of the inner routine
    Option { inner_type: Type },
    /// Specialized for `i*`
    /// Initialize a `Collector`  (requires `Default + Collector<T>`) and return the result from `Collector::finalize`.
    ZeroOrMore { collector: Type },
    /// Specialized for `i+` = `i ~ i*`.
    /// Initialize a `Collector`  (requires `From<T> + Collector<T>`), pass it to the recursive routine
    /// and return the result from `Collector::finalize`.
    OneOrMoreToplevel { collector: Type },
    /// Specialized for `i+` = `i ~ i*`.
    /// Accepts a `&mut Collector`
    OneOrMoreNested { collector: Type },
}

impl SemAct {
    /// Generate inlined expr for reduce action `terminal shift [reduce] shift shift`
    pub fn generate_inline_expr<'a, I: IntoIterator<Item = &'a Expr>>(
        &self,
        exprs: I,
        delayed_func: Option<Expr>,
    ) -> Expr {
        debug_assert_eq!(
            delayed_func.is_some(),
            matches!(self, Self::OneOrMoreToplevel { .. })
        );
        match self {
            Self::CustomizedRoutine {
                function,
                ret_type: _,
                arity: _,
            } => {
                let exprs = exprs.into_iter();
                parse_quote!(
                    #function(#(#exprs,)*)
                )
            }
            Self::Tuple => {
                let exprs = exprs.into_iter();
                parse_quote!(
                    (#(#exprs,)*)
                )
            }
            Self::Option { .. } => {
                unreachable!("Option can never be inlined, otherwise there is sequential ambiguity")
            }

            Self::ZeroOrMore { .. } => unreachable!(
                "ZeroOrMore can never be inlined, otherwise there is sequential ambiguity"
            ),

            Self::OneOrMoreNested { .. } => unreachable!(
                "OneOrMoreNested can never be inlined because it never appears in the first place"
            ),

            Self::OneOrMoreToplevel { collector } => {
                let exprs = exprs.into_iter();
                let delayed_func = delayed_func.unwrap();
                // TODO: src, offset
                parse_quote! {
                    {
                        let mut collector = #collector::from(#(#exprs)*);
                        #delayed_func(&mut collector, src, offset);
                        collector.finalize()
                    }
                }
            }
        }
    }

    /// This function is useful in the following cases:
    /// - If a shift routine is nested one or more, we does not emit the call to it immediately. Instead, we wait until
    /// [`Self::generate_inlin_expr`] is called.
    /// - If a parser routine has a semact [`Self::OneOrMoreNested`], it should be parametized by `C : Collector` in type
    /// and its has `&mut C` as its first input param.
    pub fn is_nested_one_or_more(&self) -> bool {
        matches!(self, Self::OneOrMoreNested { .. })
    }

    /// Check if we should generate loops for TCO.
    pub fn should_tco(&self) -> bool {
        matches!(self, Self::ZeroOrMore { .. } | Self::OneOrMoreNested { .. })
    }
}
