use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::path::PathBuf;
use std::rc::Rc;

pub mod core_syntax;
pub mod frontend;
pub mod lexer;
pub mod type_system;
pub mod utilities;
mod fusion;
mod nf;


pub(crate) fn unreachable_branch() -> ! {
    if cfg!(debug_assertions) {
        unreachable!("internal logic error")
    } else {
        unsafe { std::hint::unreachable_unchecked() }
    }
}
