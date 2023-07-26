// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use quote::format_ident;
use syn::Ident;

#[cfg(feature = "debug")]
use crate::debug::styled_write;
mod semact;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Tag {
    Toplevel(Ident),
    Anonymous(usize),
}

impl Tag {
    pub fn toplevel(ident: Ident) -> Self {
        Self::Toplevel(ident)
    }
    pub fn anonymous(index: usize) -> Self {
        Self::Anonymous(index)
    }
    /// Identifier of the parser routine.
    pub fn parser_name(&self) -> Ident {
        match self {
            Self::Anonymous(index) => format_ident!("__anonymous_{}", index),
            Self::Toplevel(ident) => format_ident!("parse_{}", ident),
        }
    }
}

#[cfg(feature = "debug")]
impl std::fmt::Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tag::Toplevel(ident) => write!(f, "{}", ident),
            Tag::Anonymous(index) => write!(f, "{{{}}}", index),
        }
    }
}

/// Action in the normal form.
/// If this subroutine's return value is taken, it should mark [`Action::output`] as `true`.
/// There is no need to assign an ident to a subroutine. As we are always
/// reducing from left to right, we maintain the context of which the current
/// semantic action to reduce, and always assign "__0", "__1", "__2". When a [`Reduce`] is
/// encountered, we start over from "__0".
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Action {
    Shift {
        /// Parser routine to call.
        tag: Tag,
        output: bool,
    },
    Reduce {
        /// Reduction routine to call.
        tag: Tag,
        output: bool,
    },
}

#[cfg(feature = "debug")]
impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reduce { tag, output } => {
                if *output {
                    styled_write!(f, Color::Red, "[{tag}]")
                } else {
                    styled_write!(f, Color::Blue, "[{tag}]")
                }
            }
            Self::Shift { tag, output } => {
                if *output {
                    styled_write!(f, Color::Red, "{tag}")
                } else {
                    styled_write!(f, Color::Blue, "{tag}")
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NormalForm {
    Empty(Vec<(Tag, bool)>),
    Unexpanded(Vec<Action>),
    Sequence(Ident, Vec<Action>),
}

#[cfg(feature = "debug")]
impl std::fmt::Display for NormalForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty(actions) => {
                write!(f, "ε")?;
                for (tag, output) in actions.iter() {
                    if *output {
                        styled_write!(f, Color::Red, "[{tag}]")?;
                    } else {
                        styled_write!(f, Color::Blue, "[{tag}]")?;
                    }
                }
            }
            Self::Unexpanded(actions) => {
                write!(f, "{}", actions[0])?;
                for action in &actions[1..] {
                    write!(f, " {}", action)?;
                }
            }
            Self::Sequence(terminal, actions) => {
                styled_write!(f, Color::Yellow.bold(), "{terminal}")?;
                for action in actions.iter() {
                    write!(f, " {}", action)?;
                }
            }
        }
        Ok(())
    }
}

#[cfg(all(feature = "debug", test))]
#[test]
fn debug_print_test() {
    use quote::format_ident;
    let sequence = NormalForm::Sequence(
        format_ident!("TEST"),
        vec![
            Action::Shift {
                tag: Tag::Toplevel(format_ident!("a")),
                output: false,
            },
            Action::Reduce {
                tag: Tag::Toplevel(format_ident!("b")),
                output: true,
            },
            Action::Shift {
                tag: Tag::Toplevel(format_ident!("c")),
                output: true,
            },
            Action::Reduce {
                tag: Tag::Anonymous(1),
                output: false,
            },
        ],
    );
    println!("{}", sequence);
}
