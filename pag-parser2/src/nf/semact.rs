// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::collections::HashMap;

use super::Tag;


pub type SemActTable = HashMap<Tag, SemAct>;

///
/// ```
/// trait Collector<T> : Default {
///     fn collect(&mut self, data: T);
/// }
///
/// ```

// those normal form without SemAct will be treated as plain scanner.
pub enum SemAct {
    CustomizedRoutine(syn::Block),
    /// Gather inner data. If multiple is selected, return a tuple.
    /// If only one is selected, return target data.
    Gather,
    /// Specialized for `inner?`. Return an Option of the inner routine
    Option,
    /// Specialized for `i*`
    /// Initialize a `Collector`  (requires `Collector<T>`) and return the result from `Collector::finalize`.
    ZeroOrMore,
    /// Specialized for `i+` = `i ~ i*`.
    /// Initialize a `Collector`  (requires `Collector<T>`), pass it to the recursive routine
    /// and return the result from `Collector::finalize`.
    OneOrMoreToplevel,
    /// Specialized for `i+` = `i ~ i*`.
    /// Accepts a `&mut Collector`
    OneOrMoreNested,
    /// Yield a token span,
    Token,
}
