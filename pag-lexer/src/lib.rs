// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.
#![feature(portable_simd)]
#![feature(core_intrinsics)]
#![feature(array_chunks)]

pub mod congruence;
pub mod derivative;
pub mod intervals;
pub mod lookahead;
pub mod normalization;
pub mod normalization2;
pub mod regex_tree;
pub mod regex_tree2;
pub mod vector;

#[cfg(test)]
mod tests {
    use crate::congruence::approximate_congruence_class;
    use crate::derivative::derivative;
    use crate::lookahead::LoopOptimizer;
    use crate::normalization::normalize;
    use crate::regex_tree::*;
    use crate::vector::Vector;
    use quote::quote;
    use std::rc::Rc;
    use RegexTree::*;

    #[test]
    fn it_prints_basic() {
        let a = Rc::new(RegexTree::single(b'a'));
        let b = Rc::new(RegexTree::single(b'b'));
        let ab = Rc::new(Concat(a, b));
        let alt = Rc::new(Union(ab.clone(), ab));
        println!("{}", alt);
        let derivative = derivative(alt, b'a');
        println!("{}", derivative);
        let normalized = normalize(derivative);
        println!("{}", normalized);
        println!("{:?}", approximate_congruence_class(&normalized));
    }

    #[test]
    fn renormalize_tests() {
        // concat
        let a = Rc::new(RegexTree::single(b'a'));
        let b = Rc::new(RegexTree::single(b'b'));
        let concat = Rc::new(Concat(a.clone(), b));
        let normalized = normalize(concat.clone());
        assert!(Rc::ptr_eq(&concat, &normalized));
        // kleene closure
        let kleene = Rc::new(KleeneClosure(a));
        let normalized = normalize(kleene.clone());
        assert!(Rc::ptr_eq(&kleene, &normalized));
    }

    #[test]
    fn beautify_mangle_tests() {
        // generate huge test for me
        let a = Rc::new(RegexTree::single(b'a'));
        let b = Rc::new(RegexTree::single(b'b'));
        let c = Rc::new(RegexTree::single(b'c'));
        let d = Rc::new(RegexTree::single(b'd'));
        let ba = Rc::new(Concat(b, a.clone()));
        let a_or_ba = Rc::new(Union(a, ba));
        let a_or_ba_or_c = Rc::new(Union(a_or_ba, c));
        let a_or_ba_or_c_con_d = Rc::new(KleeneClosure(Rc::new(Concat(a_or_ba_or_c, d))));
        let normalized = normalize(a_or_ba_or_c_con_d);
        let congruence = approximate_congruence_class(&normalized);
        println!("{:?}", congruence);
        let vectorized = Vector::new([normalized]);
        let mut optimizer = LoopOptimizer::new();
        println!(
            "{}",
            vectorized.generate_dfa(
                &quote!(0),
                &mut optimizer,
                &[quote!({
                    return Some(idx);
                })],
                &quote!({
                    return None;
                })
            )
        );
    }

    #[test]
    fn approximate_congruence_class_test() {
        let a = Rc::new(RegexTree::single(b'a'));
        let b = Rc::new(RegexTree::single(b'b'));
        let c = Rc::new(RegexTree::single(b'c'));
        let ba = Rc::new(Concat(b, a.clone()));
        let a_or_ba = Rc::new(Union(a, ba));
        let a_or_ba_or_c = Rc::new(Union(a_or_ba, c));
        let star = Rc::new(KleeneClosure(a_or_ba_or_c.clone()));
        let a_or_ba_or_c = Rc::new(Concat(a_or_ba_or_c, star));
        println!("{}", a_or_ba_or_c);
        let normalized = normalize(a_or_ba_or_c);
        println!("{}", normalized);
        let congruence = approximate_congruence_class(&normalized);
        println!("{:?}", congruence);
        println!();
        let vectorized = Vector::new([normalized]);
        let mut optimizer = LoopOptimizer::new();
        println!(
            "{}",
            vectorized.generate_dfa(
                &quote!(0),
                &mut optimizer,
                &[quote!({
                    return Some(idx);
                })],
                &quote!({
                    return None;
                })
            )
        );
    }
}
