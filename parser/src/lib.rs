/*
Copyright (C) 2023 Paguroidea Developpers

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at <https://mozilla.org/MPL/2.0/>.
*/

pub mod core_syntax;
pub mod frontend;
mod fusion;
mod nf;
pub mod type_system;
pub mod utilities;

pub(crate) fn unreachable_branch() -> ! {
    if cfg!(debug_assertions) {
        unreachable!("internal logic error")
    } else {
        unsafe { std::hint::unreachable_unchecked() }
    }
}
