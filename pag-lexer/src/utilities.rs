use crate::{intervals, normalization::normalize, regex_tree::RegexTree};
use smallvec::smallvec;
use std::rc::Rc;

pub fn dbg_sort<T, U, F, K>(data: T, _f: F) -> impl Iterator<Item = U>
where
    T: IntoIterator<Item = U>,
    F: FnMut(&U) -> K,
    K: Ord,
{
    #[cfg(not(debug_assertions))]
    {
        data.into_iter()
    }
    #[cfg(debug_assertions)]
    {
        let mut vec = Vec::from_iter(data);
        vec.sort_unstable_by_key(_f);
        vec.into_iter()
    }
}

fn unicode_codepoints_impl() -> Rc<RegexTree> {
    // ascii range
    let ascii = RegexTree::Set(intervals!((0x00, 0x7F)));
    // 0x80 - 0x7FF
    let two_byte = {
        let x0 = RegexTree::Set(intervals!((0b110_00000, 0b110_11111)));
        let x1 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        RegexTree::Concat(smallvec![Rc::new(x0), Rc::new(x1)])
    };
    // 0x800 - 0xCFFF
    let three_byte_1 = {
        let x0 = RegexTree::Set(intervals!((0b1110_0000, 0b1110_1100)));
        let x1 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        let x2 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        RegexTree::Concat(smallvec![Rc::new(x0), Rc::new(x1), Rc::new(x2)])
    };
    // 0xD000 - 0xD7FF
    let three_byte_2 = {
        let x0 = RegexTree::Set(intervals!((0b1110_1101, 0b1110_1101)));
        let x1 = RegexTree::Set(intervals!((0b10_000000, 0b10_011111)));
        let x2 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        RegexTree::Concat(smallvec![Rc::new(x0), Rc::new(x1), Rc::new(x2)])
    };
    // 0xE000 - 0xFFFF
    let three_byte_3 = {
        let x0 = RegexTree::Set(intervals!((0b1110_1110, 0b1110_1111)));
        let x1 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        let x2 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        RegexTree::Concat(smallvec![Rc::new(x0), Rc::new(x1), Rc::new(x2)])
    };
    // 0x10000 - 0x3FFFF
    // 0b000_010000_000000_000000
    // 0b000_111111_111111_111111
    let four_byte_1 = {
        let x0 = RegexTree::Set(intervals!((0b11110_000, 0b11110_000)));
        let x1 = RegexTree::Set(intervals!((0b10_000000, 0b10_011111)));
        let x2 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        let x3 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        RegexTree::Concat(smallvec![
            Rc::new(x0),
            Rc::new(x1),
            Rc::new(x2),
            Rc::new(x3)
        ])
    };
    // 0x40000 - 0xFFFFF
    // 0b001_000000_000000_000000
    // 0b011_111111_111111_111111
    let four_byte_2 = {
        let x0 = RegexTree::Set(intervals!((0b11110_001, 0b11110_011)));
        let x1 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        let x2 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        let x3 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        RegexTree::Concat(smallvec![
            Rc::new(x0),
            Rc::new(x1),
            Rc::new(x2),
            Rc::new(x3)
        ])
    };
    // 0x100000 - 0x10FFFF
    // 0b100_000000_000000_000000
    // 0b100_001111_111111_111111
    let four_byte_3 = {
        let x0 = RegexTree::Set(intervals!((0b11110_100, 0b11110_100)));
        let x1 = RegexTree::Set(intervals!((0b10_000000, 0b10_001111)));
        let x2 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        let x3 = RegexTree::Set(intervals!((0b10_000000, 0b10_111111)));
        RegexTree::Concat(smallvec![
            Rc::new(x0),
            Rc::new(x1),
            Rc::new(x2),
            Rc::new(x3)
        ])
    };

    let inner = normalize(Rc::new(RegexTree::Union(smallvec![
        Rc::new(ascii),
        Rc::new(two_byte),
        Rc::new(three_byte_1),
        Rc::new(three_byte_2),
        Rc::new(three_byte_3),
        Rc::new(four_byte_1),
        Rc::new(four_byte_2),
        Rc::new(four_byte_3),
    ])));

    Rc::new(RegexTree::KleeneClosure(inner))
}

thread_local! {
    static UNICODE_CODEPOINTS: Rc<RegexTree> = unicode_codepoints_impl();
}

pub fn unicode_codepoints() -> Rc<RegexTree> {
    UNICODE_CODEPOINTS.with(Rc::clone)
}
