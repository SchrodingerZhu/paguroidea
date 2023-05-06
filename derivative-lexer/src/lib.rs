#![feature(let_chains)]

use crate::intervals::{ClosedInterval, Intervals};
use std::collections::BTreeSet;
use std::fmt::{Display, Formatter};
use std::ops::RangeInclusive;
use std::rc::Rc;

pub mod class;
pub mod congruence;
pub mod derivative;
pub mod intervals;
pub mod normalization;
pub mod regex_tree;

#[cfg(test)]
mod tests {
    use crate::congruence::approximate_congruence_class;
    use crate::derivative::derivative;
    use crate::normalization::normalize;
    use crate::regex_tree::*;
    use std::rc::Rc;
    use RegexTree::*;

    #[test]
    fn it_prints_basic() {
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
