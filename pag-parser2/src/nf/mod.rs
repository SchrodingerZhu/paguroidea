// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

mod normalization;
mod semact;
mod translation;

use crate::utils::Appendix;

use std::{
    collections::{HashMap, VecDeque},
    ops::{Deref, DerefMut},
    rc::Rc,
};

use quote::format_ident;
use syn::Ident;

#[cfg(feature = "debug")]
use crate::utils::{styled, styled_write};

use self::semact::SemAct;

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
            Tag::Anonymous(index) => styled_write!(f, Style::new().italic(), "_A{index}"),
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
    /// Specialized action for tail call optimization.
    TailCall,
    /// Specialized action for passing collector to subroutines.
    PassCollector(Tag),
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
            Self::TailCall => styled_write!(f, Color::Green, "↻"),
            Self::PassCollector(tag) => styled_write!(f, Color::Green, "⇒{tag}"),
        }
    }
}

#[derive(Clone)]
pub enum AbstractType {
    /// Concrete type without any type parameter.
    Concrete(Rc<syn::Type>),
    Option(Box<Self>),
    Tuple(Vec<Self>),
    Collector(Box<Self>),
}

thread_local! {
    static UNIT_TYPE: AbstractType = AbstractType::Concrete(Rc::new(syn::parse_quote!(())));
    static SPAN_TYPE: AbstractType = AbstractType::Concrete(Rc::new(syn::parse_quote!(::pag_util::Span<'src>)));
}

impl AbstractType {
    pub fn unit_type() -> Self {
        UNIT_TYPE.with(Self::clone)
    }
    pub fn span_type() -> Self {
        SPAN_TYPE.with(Self::clone)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NormalForm {
    Empty {
        actions: Vec<Action>,
        semact: SemAct,
        ty: Appendix<AbstractType>,
    },
    Unexpanded {
        actions: Vec<Action>,
        semact: SemAct,
        ty: Appendix<AbstractType>,
    },
    Sequence {
        token: Ident,
        token_output: Option<Ident>,
        actions: Vec<Action>,
        semact: SemAct,
        ty: Appendix<AbstractType>,
    },
}

pub enum BoundTarget<'a> {
    Tag(&'a Tag),
    Token,
}

impl NormalForm {
    pub fn semact(&self) -> &SemAct {
        match self {
            Self::Empty { semact, .. } => semact,
            Self::Unexpanded { semact, .. } => semact,
            Self::Sequence { semact, .. } => semact,
        }
    }

    pub fn semact_mut(&mut self) -> &mut SemAct {
        match self {
            Self::Empty { semact, .. } => semact,
            Self::Unexpanded { semact, .. } => semact,
            Self::Sequence { semact, .. } => semact,
        }
    }

    pub fn actions(&self) -> &[Action] {
        match self {
            Self::Empty { actions, .. } => actions,
            Self::Unexpanded { actions, .. } => actions,
            Self::Sequence { actions, .. } => actions,
        }
    }

    pub fn actions_mut(&mut self) -> &mut Vec<Action> {
        match self {
            Self::Empty { actions, .. } => actions,
            Self::Unexpanded { actions, .. } => actions,
            Self::Sequence { actions, .. } => actions,
        }
    }

    pub fn ty(&self) -> &Appendix<AbstractType> {
        match self {
            Self::Empty { ty, .. } => ty,
            Self::Unexpanded { ty, .. } => ty,
            Self::Sequence { ty, .. } => ty,
        }
    }

    pub fn ty_mut(&mut self) -> &mut Appendix<AbstractType> {
        match self {
            Self::Empty { ty, .. } => ty,
            Self::Unexpanded { ty, .. } => ty,
            Self::Sequence { ty, .. } => ty,
        }
    }

    pub fn visible_bindings(&self, skip: usize) -> Box<[(&Ident, BoundTarget)]> {
        let mut acc = VecDeque::new();
        for act in self.actions().iter().rev().skip(skip) {
            match act {
                Action::Shift { tag, output } => {
                    if let Some(ident) = output {
                        acc.push_front((ident, BoundTarget::Tag(tag)));
                    }
                }
                Action::Reduce { tag, output } => {
                    if let Some(ident) = output {
                        acc.push_front((ident, BoundTarget::Tag(tag)));
                    }
                    break;
                }
                Action::PassCollector(..) => continue,
                Action::TailCall => continue,
            }
        }
        if let Self::Sequence {
            token_output: Some(tk),
            ..
        } = self
        {
            if acc.len() == self.actions().len() - skip
                && !matches!(self.actions().first(), Some(Action::Reduce { .. }))
            {
                acc.push_front((tk, BoundTarget::Token));
            }
        }
        acc.into_iter().collect()
    }
}

#[cfg(feature = "debug")]
impl std::fmt::Display for NormalForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty {
                actions, semact, ..
            } => {
                write!(f, "ε")?;
                for action in actions {
                    write!(f, "\t{}", action)?;
                }
                write!(f, "\t{{{}}}", semact)?;
            }
            Self::Unexpanded {
                actions, semact, ..
            } => {
                write!(f, "{}", actions[0])?;
                for action in &actions[1..] {
                    write!(f, "\t{}", action)?;
                }
                write!(f, "\t{{{}}}", semact)?;
            }
            Self::Sequence {
                token,
                token_output,
                actions,
                semact,
                ..
            } => {
                if let Some(tk) = token_output {
                    styled_write!(f, Color::Yellow, "{token}[{tk}]")?;
                } else {
                    styled_write!(f, Color::Yellow, "{token}")?;
                }
                for action in actions.iter() {
                    write!(f, "\t{}", action)?;
                }
                write!(f, "\t{{{}}}", semact)?;
            }
        }
        Ok(())
    }
}

#[cfg(all(feature = "debug", test))]
#[test]
fn debug_print_test() {
    use quote::format_ident;
    let sequence = NormalForm::Sequence {
        token: format_ident!("TEST"),
        token_output: Some(format_ident!("x")),
        actions: vec![
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
        semact: SemAct::Gather,
        ty: Appendix(AbstractType::Concrete(Rc::new(syn::parse_quote!(u32)))),
    };
    println!("{}", sequence);
}

/// Well, it is not the notorius firewall.
#[derive(Default, Clone)]
pub struct NFTable(HashMap<Tag, Vec<NormalForm>>);

impl Deref for NFTable {
    type Target = HashMap<Tag, Vec<NormalForm>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for NFTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
    let sequence = NormalForm::Sequence {
        token: format_ident!("TEST"),
        token_output: Some(format_ident!("x")),
        actions: vec![
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
        semact: SemAct::Gather,
        ty: Appendix(AbstractType::Concrete(Rc::new(syn::parse_quote!(u32)))),
    };
    let empty = NormalForm::Empty {
        actions: vec![
            Action::Reduce {
                tag: Tag::Toplevel(format_ident!("b")),
                output: Some(format_ident!("x")),
            },
            Action::Reduce {
                tag: Tag::Toplevel(format_ident!("c")),
                output: Some(format_ident!("y")),
            },
        ],
        semact: SemAct::Gather,
        ty: Appendix(AbstractType::Concrete(Rc::new(syn::parse_quote!(u32)))),
    };
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
