use crate::regex_tree::RegexTree;
use std::rc::Rc;

pub fn normalize(tree: Rc<RegexTree>) -> Rc<RegexTree> {
    match tree.as_ref() {
        RegexTree::Complement(r) => {
            let r = normalize(r.clone());
            match r.as_ref() {
                RegexTree::Set(x) => match x.complement() {
                    Some(y) => Rc::new(RegexTree::Set(y)),
                    None => Rc::new(RegexTree::Bottom),
                },
                RegexTree::Complement(r) => r.clone(),
                RegexTree::Top => Rc::new(RegexTree::Bottom),
                RegexTree::Bottom => Rc::new(RegexTree::Top),
                _ => Rc::new(RegexTree::Complement(r)),
            }
        }
        RegexTree::Intersection(r, s) => {
            // referential equality
            if Rc::ptr_eq(r, s) {
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
                    Rc::new(RegexTree::Intersection(r2.clone(), s)),
                )));
            }

            // will not fall in any of the above cases -- it is safe to return
            if ordering.is_lt() || matches!(s.as_ref(), RegexTree::Intersection(..)) {
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
                    Rc::new(RegexTree::Concat(r2.clone(), s)),
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
                    Rc::new(RegexTree::Union(r2.clone(), s)),
                )));
            }
            // will not fall in any of the above cases -- it is safe to return
            if ordering.is_lt() || matches!(s.as_ref(), RegexTree::Union(..)) {
                Rc::new(RegexTree::Union(r, s))
            } else {
                Rc::new(RegexTree::Union(s, r))
            }
        }
        RegexTree::Top => tree.clone(),
        RegexTree::Bottom => tree.clone(),
    }
}
