#![feature(let_chains)]
pub mod congruence;
pub mod derivative;
pub mod intervals;
pub mod normalization;
pub mod regex_tree;
pub mod vector;

#[cfg(test)]
mod tests {
    use crate::congruence::approximate_congruence_class;
    use crate::derivative::derivative;
    use crate::normalization::normalize;
    use crate::regex_tree::*;
    use crate::vector::Vector;
    use std::rc::Rc;
    use RegexTree::*;

    #[test]
    fn it_prints_basic() {
        let a = Rc::new(RegexTree::single('a'));
        let b = Rc::new(RegexTree::single('b'));
        let ab = Rc::new(Concat(a, b));
        let alt = Rc::new(Union(ab.clone(), ab));
        println!("{}", alt);
        let derivative = derivative(alt, 'a' as u32);
        println!("{}", derivative);
        let normalized = normalize(Rc::new(derivative));
        println!("{}", normalized);
        println!("{:?}", approximate_congruence_class(&normalized));
    }

    #[test]

    fn renormalize_tests() {
        // concat
        let a = Rc::new(RegexTree::single('a'));
        let b = Rc::new(RegexTree::single('b'));
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
        let a = Rc::new(RegexTree::single('a'));
        let b = Rc::new(RegexTree::single('b'));
        let c = Rc::new(RegexTree::single('c'));
        let d = Rc::new(RegexTree::single('d'));
        let ba = Rc::new(Concat(b, a.clone()));
        let a_or_ba = Rc::new(Union(a, ba));
        let a_or_ba_or_c = Rc::new(Union(a_or_ba, c));
        let a_or_ba_or_c_con_d = Rc::new(KleeneClosure(Rc::new(Concat(a_or_ba_or_c, d))));
        let normalized = normalize(a_or_ba_or_c_con_d);
        let congruence = approximate_congruence_class(&normalized);
        println!("{:?}", congruence);
        let vectorized = Vector::new([normalized].into_iter());
        println!("{}", vectorized.generate_dfa("polo".to_string()));
    }

    #[test]
    fn approximate_congruence_class_test() {
        let a = Rc::new(RegexTree::single('a'));
        let b = Rc::new(RegexTree::single('b'));
        let c = Rc::new(RegexTree::single('c'));
        let ba = Rc::new(Concat(b, a.clone()));
        let a_or_ba = Rc::new(Union(a, ba));
        let a_or_ba_or_c = Rc::new(KleeneClosure(Rc::new(Union(a_or_ba, c))));
        println!("{}", a_or_ba_or_c);
        let normalized = normalize(a_or_ba_or_c);
        println!("{}", normalized);
        let congruence = approximate_congruence_class(&normalized);
        println!("{:?}", congruence);
        println!();
        let vectorized = Vector::new([normalized].into_iter());
        println!("{}", vectorized.generate_dfa("test".to_string()));
    }
}
