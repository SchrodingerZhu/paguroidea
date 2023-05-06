#![feature(let_chains)]

use std::collections::BTreeSet;
use std::fmt::{Display, Formatter};
use std::ops::RangeInclusive;
use std::rc::Rc;
use crate::intervals::{ClosedInterval, Intervals};

pub mod intervals;
pub mod class;

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
            RegexTree::Set(Intervals::new([ClosedInterval(x as u32, x as u32)].into_iter())
                .unwrap_unchecked())
        }
    }
    pub fn range(x : RangeInclusive<char>) -> Self {
        unsafe {
            RegexTree::Set(Intervals::new([ClosedInterval(*x.start() as u32, *x.end() as u32)].into_iter())
                .unwrap_unchecked())
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

pub fn derivative(tree: Rc<RegexTree>, x: char) -> RegexTree {
    match tree.as_ref() {
        RegexTree::Set(set) => {
            if set.contains_char(x) {
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
            if let RegexTree::Set(r) = r.as_ref()
                && let RegexTree::Set(s) = s.as_ref() {
                return match r.intersection(s) {
                    Some(set) => Rc::new(RegexTree::Set(set)),
                    None => Rc::new(RegexTree::Bottom),
                }
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
            if let RegexTree::Set(r) = r.as_ref()
                && let RegexTree::Set(s) = s.as_ref() {
                return Rc::new(RegexTree::Set(r.union(s)));
            }
            // always right heavy
            if let RegexTree::Union(r1, r2) = r.as_ref() {
                return normalize(Rc::new(RegexTree::Union(
                    r1.clone(),
                    Rc::new(RegexTree::Union(r2.clone(), s.clone())),
                )));
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

pub fn approximate_congruence_class(tree: &RegexTree) -> Vec<Intervals> {
    pub fn meet(a : &[Intervals], b : &[Intervals]) -> Vec<Intervals> {
        let mut result = Vec::new();
        for x in a {
            for y in b {
                if let Some(z) = x.intersection(y) {
                    result.push(z);
                }
            }
        }
        result.sort();
        result.dedup_by(|x, y| x == y);
        result
    }
    match tree {
        RegexTree::Epsilon => vec![unsafe { intervals!((0, 0x10FFFF)).unwrap_unchecked() }],
        RegexTree::Top => { vec![unsafe { intervals!((0, 0x10FFFF)).unwrap_unchecked() }] }
        RegexTree::Bottom => { vec![unsafe { intervals!((0, 0x10FFFF)).unwrap_unchecked() }] }
        RegexTree::Set(x) => {
           let x = x.clone();
           let y = x.complement();
           if x < y {
               vec![x, y]
           } else {
               vec![y, x]
           }
        }
        RegexTree::Concat(r, s) => {
            if r.is_nullable() {
                approximate_congruence_class(r)
            } else {
                meet(&approximate_congruence_class(r), &approximate_congruence_class(s))
            }
        }
        RegexTree::KleeneClosure(r) => {
            approximate_congruence_class(r)
        }
        RegexTree::Union(r, s) => {
            meet(&approximate_congruence_class(r), &approximate_congruence_class(s))
        }
        RegexTree::Intersection(r, s) => {
            meet(&approximate_congruence_class(r), &approximate_congruence_class(s))
        }
        RegexTree::Complement(r) => {
            approximate_congruence_class(r)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_prints_basic() {
        use RegexTree::*;
        let a = Rc::new(RegexTree::single('a'));
        let b = Rc::new(RegexTree::single('b'));
        let ab = Rc::new(Concat(a.clone(), b.clone()));
        let alt = Rc::new(Union(ab.clone(), ab.clone()));
        println!("{}", alt);
        let derivative = derivative(alt, 'a');
        println!("{}", derivative);
        let normalized = normalize(Rc::new(derivative));
        println!("{}", normalized);
        println!("{:?}", approximate_congruence_class(&normalized));
    }
    #[test]
    fn approximate_congruence_class_test() {
        use RegexTree::*;
        let a = Rc::new(RegexTree::single('a'));
        let b = Rc::new(RegexTree::single('b'));
        let c = Rc::new(RegexTree::single('c'));
        let ba = Rc::new(Concat(b.clone(), a.clone()));
        let a_or_ba = Rc::new(Union(a.clone(), ba.clone()));
        let a_or_ba_or_c = Rc::new(Union(a_or_ba.clone(), c.clone()));
        println!("{}", a_or_ba_or_c);
        let normalized = normalize(a_or_ba_or_c.clone());
        println!("{}", normalized);
        let congruence = approximate_congruence_class(&normalized);
        println!("{:?}", congruence);
    }
}
