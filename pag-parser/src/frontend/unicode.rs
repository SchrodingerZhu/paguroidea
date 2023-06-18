// Copyright (c) 2023 Paguroidea Developers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use pag_lexer::normalization::normalize;
use pag_lexer::regex_tree::RegexTree;
use smallvec::smallvec;
use std::rc::Rc;

pub fn encode_char(x: char) -> Rc<RegexTree> {
    let mut buf = [0; 4];
    normalize(Rc::new(RegexTree::Concat(
        x.encode_utf8(&mut buf)
            .bytes()
            .map(|b| Rc::new(RegexTree::single(b)))
            .collect(),
    )))
}

fn full_range_2() -> Rc<RegexTree> {
    Rc::new(RegexTree::Concat(smallvec![
        Rc::new(RegexTree::range(0xc0..=0xdf)),
        Rc::new(RegexTree::range(0x80..=0xbf))
    ]))
}

fn full_range_3() -> Rc<RegexTree> {
    Rc::new(RegexTree::Concat(smallvec![
        Rc::new(RegexTree::range(0xe0..=0xef)),
        Rc::new(RegexTree::range(0x80..=0xbf)),
        Rc::new(RegexTree::range(0x80..=0xbf))
    ]))
}

fn encode_same_level1(x: char, y: char) -> Rc<RegexTree> {
    encode_same_level_expanded(1, &[x as u8], &[y as u8])
}

fn encode_same_level2(x: char, y: char) -> Rc<RegexTree> {
    let x_fst = (0xc0 | (x as u32 >> 6)) as u8;
    let x_snd = (0x80 | (x as u32 & 0x3f)) as u8;
    let y_fst = (0xc0 | (y as u32 >> 6)) as u8;
    let y_snd = (0x80 | (y as u32 & 0x3f)) as u8;
    encode_same_level_expanded(2, &[x_fst, x_snd], &[y_fst, y_snd])
}

fn encode_same_level3(x: char, y: char) -> Rc<RegexTree> {
    let x_fst = (0xe0 | (x as u32 >> 12)) as u8;
    let x_snd = (0x80 | ((x as u32 >> 6) & 0x3f)) as u8;
    let x_trd = (0x80 | (x as u32 & 0x3f)) as u8;
    let y_fst = (0xe0 | (y as u32 >> 12)) as u8;
    let y_snd = (0x80 | ((y as u32 >> 6) & 0x3f)) as u8;
    let y_trd = (0x80 | (y as u32 & 0x3f)) as u8;
    encode_same_level_expanded(3, &[x_fst, x_snd, x_trd], &[y_fst, y_snd, y_trd])
}

fn encode_same_level4(x: char, y: char) -> Rc<RegexTree> {
    let x_fst = (0xf0 | (x as u32 >> 18)) as u8;
    let x_snd = (0x80 | ((x as u32 >> 12) & 0x3f)) as u8;
    let x_trd = (0x80 | ((x as u32 >> 6) & 0x3f)) as u8;
    let x_fth = (0x80 | (x as u32 & 0x3f)) as u8;
    let y_fst = (0xf0 | (y as u32 >> 18)) as u8;
    let y_snd = (0x80 | ((y as u32 >> 12) & 0x3f)) as u8;
    let y_trd = (0x80 | ((y as u32 >> 6) & 0x3f)) as u8;
    let y_fth = (0x80 | (y as u32 & 0x3f)) as u8;
    encode_same_level_expanded(
        4,
        &[x_fst, x_snd, x_trd, x_fth],
        &[y_fst, y_snd, y_trd, y_fth],
    )
}

const ALL_BF: [u8; 4] = [0xbf, 0xbf, 0xbf, 0xbf];
const ALL_80: [u8; 4] = [0x80, 0x80, 0x80, 0x80];

fn encode_same_level_expanded(level: usize, tuple_x: &[u8], tuple_y: &[u8]) -> Rc<RegexTree> {
    if level == 1 {
        return Rc::new(RegexTree::range(tuple_x[0]..=tuple_y[0]));
    }
    if tuple_x[0] == tuple_y[0] {
        Rc::new(RegexTree::Concat(smallvec![
            Rc::new(RegexTree::single(tuple_x[0])),
            encode_same_level_expanded(level - 1, &tuple_x[1..], &tuple_y[1..]),
        ]))
    } else {
        Rc::new(RegexTree::Union(smallvec![
            Rc::new(RegexTree::Concat(smallvec![
                Rc::new(RegexTree::single(tuple_x[0])),
                encode_same_level_expanded(level - 1, &tuple_x[1..], &ALL_BF),
            ])),
            Rc::new(RegexTree::Concat(smallvec![
                Rc::new(RegexTree::range(tuple_x[0] + 1..=tuple_y[0] - 1)),
                encode_same_level_expanded(level - 1, &ALL_80, &ALL_BF),
            ])),
            Rc::new(RegexTree::Concat(smallvec![
                Rc::new(RegexTree::single(tuple_y[0])),
                encode_same_level_expanded(level - 1, &ALL_80, &tuple_y[1..]),
            ])),
        ]))
    }
}

fn encode_le_expanded(level: usize, fst_bound: u8, tuple: &[u8]) -> Rc<RegexTree> {
    if level == 1 {
        return Rc::new(RegexTree::range(fst_bound..=tuple[0]));
    }
    Rc::new(RegexTree::Union(smallvec![
        Rc::new(RegexTree::Concat(smallvec![
            Rc::new(RegexTree::single(tuple[0])),
            encode_le_expanded(level - 1, 0x80, &tuple[1..]),
        ])),
        Rc::new(RegexTree::Concat(smallvec![
            Rc::new(RegexTree::range(fst_bound..=tuple[0] - 1)),
            encode_le_expanded(level - 1, 0x80, &ALL_BF),
        ])),
    ]))
}

fn encode_ge_expanded(level: usize, fst_bound: u8, tuple: &[u8]) -> Rc<RegexTree> {
    if level == 1 {
        return Rc::new(RegexTree::range(tuple[0]..=fst_bound));
    }
    Rc::new(RegexTree::Union(smallvec![
        Rc::new(RegexTree::Concat(smallvec![
            Rc::new(RegexTree::single(tuple[0])),
            encode_ge_expanded(level - 1, 0xBF, &tuple[1..]),
        ])),
        Rc::new(RegexTree::Concat(smallvec![
            Rc::new(RegexTree::range(tuple[0] + 1..=fst_bound)),
            encode_ge_expanded(level - 1, 0xBF, &ALL_80),
        ])),
    ]))
}

fn encode_ge1(x: char) -> Rc<RegexTree> {
    encode_ge_expanded(1, 0x7F, &[x as u8])
}

fn encode_ge2(x: char) -> Rc<RegexTree> {
    let x_fst = (0xc0 | (x as u32 >> 6)) as u8;
    let x_snd = (0x80 | (x as u32 & 0x3f)) as u8;
    encode_ge_expanded(2, 0xDF, &[x_fst, x_snd])
}

fn encode_ge3(x: char) -> Rc<RegexTree> {
    let x_fst = (0xe0 | (x as u32 >> 12)) as u8;
    let x_snd = (0x80 | ((x as u32 >> 6) & 0x3f)) as u8;
    let x_trd = (0x80 | (x as u32 & 0x3f)) as u8;
    encode_ge_expanded(3, 0xEF, &[x_fst, x_snd, x_trd])
}

fn encode_le2(x: char) -> Rc<RegexTree> {
    let x_fst = (0xc0 | (x as u32 >> 6)) as u8;
    let x_snd = (0x80 | (x as u32 & 0x3f)) as u8;
    encode_le_expanded(2, 0xC0, &[x_fst, x_snd])
}

fn encode_le3(x: char) -> Rc<RegexTree> {
    let x_fst = (0xe0 | (x as u32 >> 12)) as u8;
    let x_snd = (0x80 | ((x as u32 >> 6) & 0x3f)) as u8;
    let x_trd = (0x80 | (x as u32 & 0x3f)) as u8;
    encode_le_expanded(3, 0xE0, &[x_fst, x_snd, x_trd])
}

fn encode_le4(x: char) -> Rc<RegexTree> {
    let x_fst = (0xf0 | (x as u32 >> 18)) as u8;
    let x_snd = (0x80 | ((x as u32 >> 12) & 0x3f)) as u8;
    let x_trd = (0x80 | ((x as u32 >> 6) & 0x3f)) as u8;
    let x_fth = (0x80 | (x as u32 & 0x3f)) as u8;
    encode_le_expanded(4, 0xF0, &[x_fst, x_snd, x_trd, x_fth])
}

fn try_encode_same_level(x: char, y: char) -> Option<Rc<RegexTree>> {
    match (x as u32, y as u32) {
        (0x00..=0x7F, 0x00..=0x7F) => Some(encode_same_level1(x, y)),
        (0x80..=0x7FF, 0x80..=0x7FF) => Some(encode_same_level2(x, y)),
        (0x800..=0xFFFF, 0x800..=0xFFFF) => Some(encode_same_level3(x, y)),
        (0x10000..=0x10FFFF, 0x10000..=0x10FFFF) => Some(encode_same_level4(x, y)),
        _ => None,
    }
}

pub fn encode_range(x: char, y: char) -> Rc<RegexTree> {
    if let Some(tree) = try_encode_same_level(x, y) {
        return normalize(tree);
    }
    let ranges = match (x as u32, y as u32) {
        (0x00..=0x7F, 0x80..=0x7FF) => vec![encode_ge1(x), encode_le2(y)],
        (0x00..=0x7F, 0x800..=0xFFFF) => vec![encode_ge1(x), full_range_2(), encode_le3(y)],
        (0x00..=0x7F, 0x10000..=0x10FFFF) => {
            vec![encode_ge1(x), full_range_2(), full_range_3(), encode_le4(y)]
        }
        (0x80..=0x7FF, 0x800..=0xFFFF) => vec![encode_ge2(x), encode_le3(y)],
        (0x80..=0x7FF, 0x10000..=0x10FFFF) => vec![encode_ge2(x), full_range_3(), encode_le4(y)],
        (0x800..=0xFFFF, 0x10000..=0x10FFFF) => vec![encode_ge3(x), encode_le4(y)],
        _ => unreachable!(),
    };
    // fold union
    normalize(Rc::new(RegexTree::Union(ranges.into_iter().collect())))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_encode_char() {
        assert_eq!(encode_char('a').to_string(), "a");
        assert_eq!(encode_char('b').to_string(), "b");
        assert_eq!(encode_char('æ').to_string(), r"(\xc3 ~ \xa6)");
        assert_eq!(encode_char('我').to_string(), r"(\xe6 ~ \x88 ~ \x91)");
    }

    #[test]
    fn test_encode_range() {
        assert_eq!(encode_range('a', 'a').to_string(), "a");
        assert_eq!(encode_range('a', 'b').to_string(), "[a, b]");
        assert_eq!(
            encode_range('\u{80}', '\u{88}').to_string(),
            r"(\xc2 ~ [\x80, \x88])"
        );
        assert_eq!(
            encode_range('\u{81}', '\u{7FA}').to_string(),
            r"((\xc2 ~ [\x81, \xbf]) ∪ ([\xc3, \xde] ~ [\x80, \xbf]) ∪ (\xdf ~ [\x80, \xba]))"
        );
        assert_eq!(
            encode_range('\u{800}', '\u{808}').to_string(),
            r"(\xe0 ~ \xa0 ~ [\x80, \x88])"
        );
        assert_eq!(
            encode_range('\u{881}', '\u{FFA}').to_string(),
            r"(\xe0 ~ ((\xa2 ~ [\x81, \xbf]) ∪ ([\xa3, \xbe] ~ [\x80, \xbf]) ∪ (\xbf ~ [\x80, \xba])))"
        );
        assert_eq!(
            encode_range('\u{901}', '\u{FF00}').to_string(),
            "((\\xe0 ~ ((\\xa4 ~ [\\x81, \\xbf]) ∪ ([\\xa5, \\xbe] ~ [\\x80, \\xbf]) ∪ (\\xbf ~ [\\x80, \\xbf]))) ∪ ([\\xe1, \\xee] ~ ((\\x80 ~ [\\x80, \\xbf]) ∪ ([\\x81, \\xbe] ~ [\\x80, \\xbf]) ∪ (\\xbf ~ [\\x80, \\xbf]))) ∪ (\\xef ~ ((\\x80 ~ [\\x80, \\xbf]) ∪ ([\\x81, \\xbb] ~ [\\x80, \\xbf]) ∪ (\\xbc ~ \\x80))))"
        );
        assert_eq!(
            encode_range('a', '\u{90}').to_string(),
            r"([a, \x7f] ∪ ([\xc0, \xc1] ~ [\x80, \xbf]) ∪ (\xc2 ~ [\x80, \x90]))"
        );
        assert_eq!(
            encode_range('a', '\u{801}').to_string(),
            r"([a, \x7f] ∪ ([\xc0, \xdf] ~ [\x80, \xbf]) ∪ (\xe0 ~ (([\x80, \x9f] ~ [\x80, \xbf]) ∪ (\xa0 ~ [\x80, \x81]))))"
        );
        assert_eq!(
            encode_range('\u{99}', '\u{2771}').to_string(),
            "((\\xc2 ~ [\\x99, \\xbf]) ∪ ([\\xc3, \\xdf] ~ [\\x80, \\xbf]) ∪ ([\\xe0, \\xe1] ~ (([\\x80, \\xbe] ~ [\\x80, \\xbf]) ∪ (\\xbf ~ [\\x80, \\xbf]))) ∪ (\\xe2 ~ (([\\x80, \\x9c] ~ [\\x80, \\xbf]) ∪ (\\x9d ~ [\\x80, \\xb1]))))"
        )
    }
}
