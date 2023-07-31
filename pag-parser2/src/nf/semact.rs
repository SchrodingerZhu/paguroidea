// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use crate::frontend::{CodeBlock, ParserExpr};

///
/// ```
/// trait Collector<T> : Default {
///     fn collect(&mut self, data: T);
/// }
///
/// ```

// those normal form without SemAct will be treated as plain scanner.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SemAct {
    Customized(CodeBlock),
    /// Gather inner data. If multiple is selected, return a tuple.
    /// If only one is selected, return target data.
    Gather,
    /// Specialized for `inner?`. Return an Option of the inner routine
    OptSome,
    OptNone,
    /// Specialized for `i*`
    /// Initialize a `Collector`  (requires `Collector<T>`) and return the result from `Collector::finalize`.
    ZeroOrMoreFinish,
    ZeroOrMoreCollect,
    /// Specialized for `i+` = `i ~ i*`.
    /// Initialize a `Collector`  (requires `Collector<T>`), pass it to the recursive routine
    /// and return the result from `Collector::finalize`.
    OneOrMoreToplevel,
    /// Specialized for `i+` = `i ~ i*`.
    /// Accepts a `&mut Collector`
    OneOrMoreNestedCollect,
    OneOrMoreNestedFinish,
    /// Yield a token span,
    Token,
    /// Recognize without generate any data.
    Recognize,
}

#[cfg(feature = "debug")]
impl std::fmt::Display for SemAct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemAct::Customized(x) => write!(f, "{:?}", std::rc::Rc::as_ptr(&x.0)),
            SemAct::Gather => write!(f, "Gather"),
            SemAct::OptSome => write!(f, "OptSome"),
            SemAct::OptNone => write!(f, "OptNone"),
            SemAct::ZeroOrMoreCollect => write!(f, "ZeroOrMoreCollect"),
            SemAct::ZeroOrMoreFinish => write!(f, "ZeroOrMoreFinish"),
            SemAct::OneOrMoreToplevel => write!(f, "OneOrMoreToplevel"),
            SemAct::OneOrMoreNestedCollect => write!(f, "OneOrMoreNestedCollect"),
            SemAct::OneOrMoreNestedFinish => write!(f, "OneOrMoreNestedFinish"),
            SemAct::Token => write!(f, "Token"),
            SemAct::Recognize => write!(f, "Recognize"),
        }
    }
}
