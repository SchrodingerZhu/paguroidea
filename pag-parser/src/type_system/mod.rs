// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

mod binding_proxy;
mod context;
mod fixpoint;
mod type_check;

pub use fixpoint::infer_fixpoints;
pub use type_check::{type_check, Type, TypeError};
