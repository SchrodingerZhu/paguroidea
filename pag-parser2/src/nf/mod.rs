// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

mod inference;
mod semact;

use std::{
    collections::{HashMap, VecDeque},
    ops::{Deref},
};

use quote::format_ident;
use syn::Ident;

#[cfg(feature = "debug")]
use crate::debug::{styled, styled_write};

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
            Tag::Toplevel(ident) => write!(f, "{ident}"),
            Tag::Anonymous(index) => styled_write!(f, Style::new().italic(), "_{index}"),
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
        output: Option<Ident>,
    },
    Reduce {
        /// Reduction routine to call.
        tag: Tag,
        output: Option<Ident>,
    },
}

#[cfg(feature = "debug")]
impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reduce { tag, output } => {
                if let Some(name) = output {
                    styled_write!(f, Color::Blue, "{tag}[{name}]")
                } else {
                    styled_write!(f, Color::Blue, "{tag}")
                }
            }
            Self::Shift { tag, output } => {
                if let Some(name) = output {
                    styled_write!(f, Color::Red, "{tag}[{name}]")
                } else {
                    styled_write!(f, Color::Red, "{tag}")
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NormalForm {
    Empty(Vec<(Tag, Option<Ident>)>),
    Unexpanded(Vec<Action>),
    Sequence(Ident, Vec<Action>),
}

impl NormalForm {
    pub fn visible_bindings(&self, skip: usize) -> Vec<(&Ident, &Tag)> {
        match self {
            Self::Empty(actions) => actions
                .last()
                .and_then(|(tag, ident)| Some((ident.as_ref()?, tag)))
                .into_iter()
                .collect(),
            Self::Unexpanded(actions) | Self::Sequence(_, actions) => {
                let mut acc = VecDeque::new();
                for act in actions.iter().rev().skip(skip) {
                    match act {
                        Action::Shift { tag, output } => {
                            if let Some(ident) = output {
                                acc.push_front((ident, tag));
                            }
                        }
                        Action::Reduce { tag, output } => {
                            if let Some(ident) = output {
                                acc.push_front((ident, tag));
                            }
                            break;
                        }
                    }
                }
                acc.into_iter().collect()
            }
        }
    }
}

#[cfg(feature = "debug")]
impl std::fmt::Display for NormalForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty(actions) => {
                write!(f, "ε")?;
                for (tag, output) in actions.iter() {
                    if let Some(name) = output {
                        styled_write!(f, Color::Blue, "\t{tag}[{name}]")?;
                    } else {
                        styled_write!(f, Color::Blue, "\t{tag}")?;
                    }
                }
            }
            Self::Unexpanded(actions) => {
                write!(f, "{}", actions[0])?;
                for action in &actions[1..] {
                    write!(f, "\t{}", action)?;
                }
            }
            Self::Sequence(terminal, actions) => {
                styled_write!(f, Color::Yellow, "{terminal}")?;
                for action in actions.iter() {
                    write!(f, "\t{}", action)?;
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
                output: None,
            },
            Action::Reduce {
                tag: Tag::Toplevel(format_ident!("b")),
                output: Some(format_ident!("x")),
            },
            Action::Shift {
                tag: Tag::Toplevel(format_ident!("c")),
                output: Some(format_ident!("y")),
            },
            Action::Reduce {
                tag: Tag::Anonymous(1),
                output: None,
            },
        ],
    );
    println!("{}", sequence);
}

/// Well, it is not the notorius firewall.
pub struct NFTable(HashMap<Tag, Vec<NormalForm>>);

impl Deref for NFTable {
    type Target = HashMap<Tag, Vec<NormalForm>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(feature = "debug")]
impl std::fmt::Display for NFTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = term_size::dimensions().map(|x| x.0).unwrap_or(0);
        writeln!(f, "┏{}┓", "━".repeat(width.saturating_sub(2)))?;
        writeln!(
            f,
            "\t{}\t{}\t{}\t{}\n",
            styled!(Color::Red.bold(), "Shift"),
            styled!(Color::Blue.bold(), "Reduce"),
            styled!(Style::new().bold(), "[Output]"),
            styled!(Style::new().italic().bold(), "Anonymous"),
        )?;
        for (tag, forms) in self.iter() {
            writeln!(
                f,
                "\t{}\t=\t{}",
                styled!(Style::new().underline(), "{tag}"),
                forms[0]
            )?;
            for form in &forms[1..] {
                writeln!(f, "\t\t|\t{}", form)?;
            }
            writeln!(f)?;
        }
        writeln!(f, "┗{}┛", "━".repeat(width.saturating_sub(2)))
    }
}

#[cfg(all(feature = "debug", test))]
#[test]
fn debug_print_nf_table() {
    use quote::format_ident;
    let sequence = NormalForm::Sequence(
        format_ident!("TEST"),
        vec![
            Action::Shift {
                tag: Tag::Toplevel(format_ident!("a")),
                output: None,
            },
            Action::Reduce {
                tag: Tag::Toplevel(format_ident!("b")),
                output: Some(format_ident!("x")),
            },
            Action::Shift {
                tag: Tag::Toplevel(format_ident!("c")),
                output: Some(format_ident!("y")),
            },
            Action::Reduce {
                tag: Tag::Anonymous(1),
                output: None,
            },
        ],
    );
    let empty = NormalForm::Empty(vec![
        (Tag::Toplevel(format_ident!("a")), None),
        (Tag::Toplevel(format_ident!("b")), Some(format_ident!("x"))),
    ]);
    let table = NFTable(
        vec![
            (
                Tag::Toplevel(format_ident!("TEST1")),
                vec![sequence.clone(), empty.clone()],
            ),
            (Tag::Toplevel(format_ident!("TEST2")), vec![sequence, empty]),
        ]
        .into_iter()
        .collect(),
    );
    println!("{}", table);
}
