





pub mod core_syntax;
pub mod frontend;
mod fusion;
pub mod lexer;
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
