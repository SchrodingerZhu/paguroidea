use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, Range};
use std::path::PathBuf;
use std::rc::Rc;

pub mod type_system;
use type_system::Type;

#[derive(Clone, Debug)]
pub struct UniqueSymbol(Rc<String>);

impl Hash for UniqueSymbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}

impl Display for UniqueSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq for UniqueSymbol {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for UniqueSymbol {}

pub trait Token: Eq + Hash + Debug + Clone {}

#[derive(Debug, Clone)]
pub enum Location {
    File {
        path: Rc<PathBuf>,
        span: Range<usize>,
    },
    Stdin {
        span: Range<usize>,
    },
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::File { path, span } => {
                write!(f, "{}:{}-{}", path.display(), span.start, span.end)
            }
            Location::Stdin { span } => write!(f, "<stdin>:{}-{}", span.start, span.end),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Term<T: Token> {
    Epsilon,
    Sequence(Rc<Term<T>>, Rc<Term<T>>),
    Token(T),
    Bottom,
    Alternative(Rc<Term<T>>, Rc<Term<T>>),
    Fix(UniqueSymbol, Rc<Term<T>>),
    Variable(UniqueSymbol),
    SrcPos(Rc<Term<T>>, Location),
    WellTyped(Rc<Term<T>>, Type<T>),
}

pub(crate) fn unreachable_branch() -> ! {
    if cfg!(debug_assertions) {
        unreachable!("internal logic error")
    } else {
        unsafe { std::hint::unreachable_unchecked() }
    }
}
