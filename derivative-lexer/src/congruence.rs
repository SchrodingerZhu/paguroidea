use crate::intervals;
use crate::intervals::Intervals;
use crate::regex_tree::RegexTree;

pub fn meet(a: &[Intervals], b: &[Intervals]) -> Vec<Intervals> {
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

pub fn approximate_congruence_class(tree: &RegexTree) -> Vec<Intervals> {
    match tree {
        RegexTree::Epsilon => vec![unsafe { intervals!((0, 0x10FFFF)).unwrap_unchecked() }],
        RegexTree::Top => {
            vec![unsafe { intervals!((0, 0x10FFFF)).unwrap_unchecked() }]
        }
        RegexTree::Bottom => {
            vec![unsafe { intervals!((0, 0x10FFFF)).unwrap_unchecked() }]
        }
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
                meet(
                    &approximate_congruence_class(r),
                    &approximate_congruence_class(s),
                )
            }
        }
        RegexTree::KleeneClosure(r) => approximate_congruence_class(r),
        RegexTree::Union(r, s) => meet(
            &approximate_congruence_class(r),
            &approximate_congruence_class(s),
        ),
        RegexTree::Intersection(r, s) => meet(
            &approximate_congruence_class(r),
            &approximate_congruence_class(s),
        ),
        RegexTree::Complement(r) => approximate_congruence_class(r),
    }
}
