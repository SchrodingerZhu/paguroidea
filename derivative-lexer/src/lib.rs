#![feature(let_chains)]

use std::collections::BTreeSet;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
mod intervals;

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone, Hash)]
pub enum RegexTree {
    Top,
    // any character
    Bottom,
    // no character
    Set(RegexSet),
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

impl Display for RegexSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegexSet::Single(x) => {
                write!(f, "'{}'", x.escape_debug())
            }
            RegexSet::Range(x, y) => {
                write!(f, "'{}'..'{}'", x.escape_debug(), y.escape_debug())
            }
            RegexSet::Discrete(s) => s
                .iter()
                .fold(write!(f, "{{ "), |acc, x| {
                    acc.and_then(|_| write!(f, "'{}' ", x.escape_debug()))
                })
                .and_then(|_| write!(f, "}}")),
            RegexSet::Alphabetic => {
                write!(f, "[:alphabetic:]")
            }
            RegexSet::Numeric => {
                write!(f, "[:numeric:]")
            }
            RegexSet::Lowercase => {
                write!(f, "[:lowercase:]")
            }
            RegexSet::Uppercase => {
                write!(f, "[:uppercase:]")
            }
            RegexSet::Whitespace => {
                write!(f, "[:whitespace:]")
            }
        }
    }
}

impl RegexTree {
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

pub fn derivative(tree: Rc<RegexTree>, x: char) -> RegexTree {
    match tree.as_ref() {
        RegexTree::Set(set) => {
            if set.contains(x) {
                RegexTree::Epsilon
            } else {
                RegexTree::Bottom
            }
        }
        RegexTree::Epsilon => RegexTree::Bottom,
        RegexTree::Concat(r, s) => {
            let lhs = RegexTree::Concat(Rc::new(derivative(r.clone(), x)), s.clone());
            if r.is_nullable() {
                RegexTree::Union(Rc::new(lhs), Rc::new(derivative(s.clone(), x)))
            } else {
                lhs
            }
        }
        RegexTree::KleeneClosure(r) => {
            RegexTree::Concat(Rc::new(derivative(r.clone(), x)), tree.clone())
        }
        RegexTree::Union(r, s) => RegexTree::Union(
            Rc::new(derivative(r.clone(), x)),
            Rc::new(derivative(s.clone(), x)),
        ),
        RegexTree::Intersection(r, s) => RegexTree::Intersection(
            Rc::new(derivative(r.clone(), x)),
            Rc::new(derivative(s.clone(), x)),
        ),
        RegexTree::Complement(r) => RegexTree::Complement(Rc::new(derivative(r.clone(), x))),
        RegexTree::Top => RegexTree::Epsilon,
        RegexTree::Bottom => RegexTree::Bottom,
    }
}

pub fn normalize(tree: Rc<RegexTree>) -> Rc<RegexTree> {
    match tree.as_ref() {
        RegexTree::Complement(r) => {
            let r = normalize(r.clone());
            match r.as_ref() {
                RegexTree::Complement(r) => r.clone(),
                RegexTree::Top => Rc::new(RegexTree::Bottom),
                RegexTree::Bottom => Rc::new(RegexTree::Top),
                _ => Rc::new(RegexTree::Complement(r)),
            }
        }
        RegexTree::Intersection(r, s) => {
            // referential equality
            if Rc::ptr_eq(&r, &s) {
                return normalize(r.clone());
            }
            let r = normalize(r.clone());
            let s = normalize(s.clone());
            // empty & r = empty
            if matches!(r.as_ref(), RegexTree::Bottom) {
                return r;
            }
            if matches!(s.as_ref(), RegexTree::Bottom) {
                return s;
            }
            // any & r = r
            if matches!(s.as_ref(), RegexTree::Top) {
                return r;
            }
            if matches!(r.as_ref(), RegexTree::Top) {
                return s;
            }

            let ordering = r.cmp(&s);
            if ordering.is_eq() {
                return r;
            }
            if let RegexTree::Set(m) = r.as_ref()
                && let RegexTree::Set(n) = s.as_ref()
                && let Some(x) = m.intersection(n)
            {
                return Rc::new(x);
            }
            // always right heavy
            if let RegexTree::Intersection(r1, r2) = r.as_ref() {
                return normalize(Rc::new(RegexTree::Intersection(
                    r1.clone(),
                    Rc::new(RegexTree::Intersection(r2.clone(), s.clone())),
                )));
            }

            // will not fall in any of the above cases -- it is safe to return
            if ordering.is_lt() {
                Rc::new(RegexTree::Intersection(r, s))
            } else {
                Rc::new(RegexTree::Intersection(s, r))
            }
        }
        RegexTree::Set(_) => tree.clone(),
        RegexTree::Epsilon => tree.clone(),
        RegexTree::Concat(r, s) => {
            let r = normalize(r.clone());
            let s = normalize(s.clone());
            // empty . r = empty
            if matches!(r.as_ref(), RegexTree::Bottom) {
                return r;
            }
            if matches!(s.as_ref(), RegexTree::Bottom) {
                return s;
            }
            // r. epsilon = r
            if matches!(s.as_ref(), RegexTree::Epsilon) {
                return r;
            }
            // epsilon . s = s
            if matches!(r.as_ref(), RegexTree::Epsilon) {
                return s;
            }
            if let RegexTree::Concat(r1, r2) = r.as_ref() {
                return normalize(Rc::new(RegexTree::Concat(
                    r1.clone(),
                    Rc::new(RegexTree::Concat(r2.clone(), s.clone())),
                )));
            }
            Rc::new(RegexTree::Concat(r, s))
        }
        RegexTree::KleeneClosure(r) => {
            let r = normalize(r.clone());
            match r.as_ref() {
                RegexTree::KleeneClosure(_) => r.clone(),
                RegexTree::Bottom => Rc::new(RegexTree::Epsilon),
                RegexTree::Epsilon => Rc::new(RegexTree::Epsilon),
                _ => Rc::new(RegexTree::KleeneClosure(r)),
            }
        }
        RegexTree::Union(r, s) => {
            // referential equality
            if Rc::ptr_eq(r, s) {
                return normalize(r.clone());
            }
            let r = normalize(r.clone());
            let s = normalize(s.clone());

            // empty | r = r
            if matches!(r.as_ref(), RegexTree::Bottom) {
                return s;
            }
            if matches!(s.as_ref(), RegexTree::Bottom) {
                return r;
            }
            // any | r = any
            if matches!(s.as_ref(), RegexTree::Top) {
                return s;
            }
            if matches!(r.as_ref(), RegexTree::Top) {
                return r;
            }
            let ordering = r.cmp(&s);
            if ordering.is_eq() {
                return r;
            }
            // always right heavy
            if let RegexTree::Union(r1, r2) = r.as_ref() {
                return normalize(Rc::new(RegexTree::Union(
                    r1.clone(),
                    Rc::new(RegexTree::Union(r2.clone(), s.clone())),
                )));
            }

            if let RegexTree::Set(m) = r.as_ref()
                && let RegexTree::Set(n) = s.as_ref()
                && let Some(x) = m.union(n)
            {
                return Rc::new(RegexTree::Set(x));
            }
            // will not fall in any of the above cases -- it is safe to return
            if ordering.is_lt() {
                Rc::new(RegexTree::Union(r, s))
            } else {
                Rc::new(RegexTree::Union(s, r))
            }
        }
        RegexTree::Top => tree.clone(),
        RegexTree::Bottom => tree.clone(),
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone, Hash)]
pub enum RegexSet {
    Single(char),
    Range(char, char),
    Discrete(BTreeSet<char>),
    Alphabetic,
    Numeric,
    Lowercase,
    Uppercase,
    Whitespace,
}

impl RegexSet {
    pub fn contains(&self, x: char) -> bool {
        match self {
            RegexSet::Single(c) => x == *c,
            RegexSet::Range(l, r) => x >= *l && x <= *r,
            RegexSet::Discrete(set) => set.contains(&x),
            RegexSet::Alphabetic => x.is_alphabetic(),
            RegexSet::Numeric => x.is_numeric(),
            RegexSet::Lowercase => x.is_lowercase(),
            RegexSet::Uppercase => x.is_uppercase(),
            RegexSet::Whitespace => x.is_whitespace(),
        }
    }

    pub fn intersection(&self, other: &Self) -> Option<RegexTree> {
        match (self, other) {
            (x, y) if x == y => Some(RegexTree::Set(x.clone())),
            (RegexSet::Single(x), y) | (y, RegexSet::Single(x)) => {
                if y.contains(*x) {
                    Some(RegexTree::Set(RegexSet::Single(*x)))
                } else {
                    Some(RegexTree::Bottom)
                }
            }
            (RegexSet::Discrete(x), y) | (y, RegexSet::Discrete(x)) => {
                let set = x
                    .iter()
                    .copied()
                    .filter(|i| y.contains(*i))
                    .collect::<BTreeSet<_>>();
                if set.is_empty() {
                    Some(RegexTree::Bottom)
                } else {
                    Some(RegexTree::Set(RegexSet::Discrete(set)))
                }
            }
            (RegexSet::Range(l1, r1), RegexSet::Range(l2, r2)) => {
                // change if two ranges overlap
                if l1 <= l2 && r1 >= l2 || l2 <= l1 && r2 >= l1 {
                    Some(RegexTree::Set(RegexSet::Range(*l1.max(l2), *r1.min(r2))))
                } else {
                    Some(RegexTree::Bottom)
                }
            }
            (RegexSet::Lowercase, RegexSet::Uppercase) => Some(RegexTree::Bottom),
            (RegexSet::Uppercase, RegexSet::Lowercase) => Some(RegexTree::Bottom),
            (RegexSet::Alphabetic, RegexSet::Numeric) => Some(RegexTree::Bottom),
            (RegexSet::Numeric, RegexSet::Alphabetic) => Some(RegexTree::Bottom),
            (RegexSet::Whitespace, RegexSet::Alphabetic) => Some(RegexTree::Bottom),
            (RegexSet::Alphabetic, RegexSet::Whitespace) => Some(RegexTree::Bottom),
            (RegexSet::Whitespace, RegexSet::Numeric) => Some(RegexTree::Bottom),
            (RegexSet::Numeric, RegexSet::Whitespace) => Some(RegexTree::Bottom),
            // maybe more cases
            _ => todo!(),
        }
    }
    // heuristic
    pub fn union(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (RegexSet::Single(x), y) | (y, RegexSet::Single(x)) if y.contains(*x) => {
                Some(y.clone())
            }

            (RegexSet::Discrete(x), y) | (y, RegexSet::Discrete(x))
                if x.iter().all(|x| y.contains(*x)) =>
            {
                Some(y.clone())
            }

            (RegexSet::Range(l1, r1), RegexSet::Range(l2, r2)) => {
                // change if two ranges overlap
                if l1 <= l2 && r1 >= l2 || l2 <= l1 && r2 >= l1 {
                    Some(RegexSet::Range(*l1.min(l2), *r1.max(r2)))
                } else {
                    None
                }
            }

            // single, single to discrete
            (RegexSet::Single(c1), RegexSet::Single(c2)) => {
                Some(RegexSet::Discrete([*c1, *c2].into()))
            }

            (RegexSet::Discrete(set), RegexSet::Single(c))
            | (RegexSet::Single(c), RegexSet::Discrete(set)) => {
                let mut set = set.clone();
                set.insert(*c);
                Some(RegexSet::Discrete(set))
            }

            _ => None,
        }
    }
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_prints_basic() {
        let a = Rc::new(RegexTree::Set(RegexSet::Single('a')));
        let b = Rc::new(RegexTree::Set(RegexSet::Single('b')));
        let ab = Rc::new(RegexTree::Concat(a.clone(), b.clone()));
        let alt = Rc::new(RegexTree::Union(ab.clone(), ab.clone()));
        println!("{}", alt);
        let derivative = derivative(alt, 'a');
        println!("{}", derivative);
        let normalized = normalize(Rc::new(derivative));
        println!("{}", normalized);
    }
}
