use crate::intervals::{ClosedInterval, Intervals};
use std::fmt::{Display, Formatter};
use std::ops::RangeInclusive;
use std::rc::Rc;

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone, Hash)]
pub enum RegexTree {
    Top,
    // any character
    Bottom,
    // no character
    Set(Intervals),
    Epsilon,
    Concat(Rc<RegexTree>, Rc<RegexTree>),
    KleeneClosure(Rc<RegexTree>),
    Union(Rc<RegexTree>, Rc<RegexTree>),
    Intersection(Rc<RegexTree>, Rc<RegexTree>),
    Complement(Rc<RegexTree>),
}

impl Display for RegexTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegexTree::Top => {
                write!(f, "⊤")
            }
            RegexTree::Bottom => {
                write!(f, "⊥")
            }
            RegexTree::Set(x) => {
                write!(f, "{}", x)
            }
            RegexTree::Epsilon => {
                write!(f, "ε")
            }
            RegexTree::Concat(x, y) => {
                write!(f, "({} ~ {})", x, y)
            }
            RegexTree::KleeneClosure(x) => {
                write!(f, "({})*", x)
            }
            RegexTree::Union(x, y) => {
                write!(f, "({} ∪ {})", x, y)
            }
            RegexTree::Intersection(x, y) => {
                write!(f, "({} ∩ {})", x, y)
            }
            RegexTree::Complement(x) => {
                write!(f, "¬({})", x)
            }
        }
    }
}

impl RegexTree {
    pub fn single(x: char) -> Self {
        unsafe {
            RegexTree::Set(
                Intervals::new([ClosedInterval(x as u32, x as u32)].into_iter()).unwrap_unchecked(),
            )
        }
    }
    pub fn range(x: RangeInclusive<char>) -> Self {
        unsafe {
            RegexTree::Set(
                Intervals::new([ClosedInterval(*x.start() as u32, *x.end() as u32)].into_iter())
                    .unwrap_unchecked(),
            )
        }
    }
    pub fn mangle(&self)-> String {
        match self {
            RegexTree::Top => {
                "T".to_string()
            }
            RegexTree::Bottom => {
                "B".to_string()
            }
            RegexTree::Set(x) => {
                x.mangle()
            }
            RegexTree::Epsilon => {
                "E".to_string()
            }
            RegexTree::Concat(x, y) => {
                let x = x.mangle();
                let y = y.mangle();
                format!("C{}{}{}{}", x.len(), x, y.len(), y)
            }
            RegexTree::KleeneClosure(x) => {
                let x = x.mangle();
                format!("K{}{}", x.len(), x)
            }
            RegexTree::Union(x, y) => {
                let x = x.mangle();
                let y = y.mangle();
                format!("U{}{}{}{}", x.len(), x, y.len(), y)
            }
            RegexTree::Intersection(x, y) => {
                let x = x.mangle();
                let y = y.mangle();
                format!("I{}{}{}{}", x.len(), x, y.len(), y)
            }
            RegexTree::Complement(x) => {
                let x = x.mangle();
                format!("N{}{}", x.len(), x)
            }
        }
    }
    pub fn is_nullable(&self) -> bool {
        match self {
            RegexTree::Top => false,
            RegexTree::Bottom => false,
            RegexTree::Set(_) => false,
            RegexTree::Epsilon => true,
            RegexTree::Concat(left, right) => left.is_nullable() && right.is_nullable(),
            RegexTree::KleeneClosure(_) => true,
            RegexTree::Union(left, right) => left.is_nullable() || right.is_nullable(),
            RegexTree::Intersection(left, right) => left.is_nullable() && right.is_nullable(),
            RegexTree::Complement(r) => !r.is_nullable(),
        }
    }
}
