use pag_lexer::normalization::normalize;
use pag_lexer::regex_tree::RegexTree;
use std::rc::Rc;

pub fn encode_char(x: char) -> Rc<RegexTree> {
    let c = x as u32;
    Rc::new(match c {
        0x000000..=0x00007F => RegexTree::single(c as u8),
        0x000080..=0x0007FF => {
            let fst = (0xc0 | (c >> 6)) as u8;
            let snd = (0x80 | (c & 0x3f)) as u8;
            RegexTree::Concat(
                Rc::new(RegexTree::single(fst)),
                Rc::new(RegexTree::single(snd)),
            )
        }
        0x000800..=0x00FFFF => {
            let fst = (0xe0 | (c >> 12)) as u8;
            let snd = (0x80 | ((c >> 6) & 0x3f)) as u8;
            let trd = (0x80 | (c & 0x3f)) as u8;
            RegexTree::Concat(
                Rc::new(RegexTree::Concat(
                    Rc::new(RegexTree::single(fst)),
                    Rc::new(RegexTree::single(snd)),
                )),
                Rc::new(RegexTree::single(trd)),
            )
        }
        0x010000..=0x10FFFF => {
            let fst = (0xf0 | (c >> 18)) as u8;
            let snd = (0x80 | ((c >> 12) & 0x3f)) as u8;
            let trd = (0x80 | ((c >> 6) & 0x3f)) as u8;
            let fth = (0x80 | (c & 0x3f)) as u8;
            RegexTree::Concat(
                Rc::new(RegexTree::Concat(
                    Rc::new(RegexTree::Concat(
                        Rc::new(RegexTree::single(fst)),
                        Rc::new(RegexTree::single(snd)),
                    )),
                    Rc::new(RegexTree::single(trd)),
                )),
                Rc::new(RegexTree::single(fth)),
            )
        }
        _ => {
            unreachable!("Invalid unicode character: {}", x)
        }
    })
}

fn full_range_2() -> Rc<RegexTree> {
    Rc::new(RegexTree::Concat(
        Rc::new(RegexTree::range(0xc0..=0xdf)),
        Rc::new(RegexTree::range(0x80..=0xbf)),
    ))
}

fn full_range_3() -> Rc<RegexTree> {
    Rc::new(RegexTree::Concat(
        Rc::new(RegexTree::Concat(
            Rc::new(RegexTree::range(0xe0..=0xef)),
            Rc::new(RegexTree::range(0x80..=0xbf)),
        )),
        Rc::new(RegexTree::range(0x80..=0xbf)),
    ))
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
        Rc::new(RegexTree::Concat(
            Rc::new(RegexTree::single(tuple_x[0])),
            encode_same_level_expanded(level - 1, &tuple_x[1..], &tuple_y[1..]),
        ))
    } else {
        Rc::new(RegexTree::Union(
            Rc::new(RegexTree::Union(
                Rc::new(RegexTree::Concat(
                    Rc::new(RegexTree::single(tuple_x[0])),
                    encode_same_level_expanded(level - 1, &tuple_x[1..], &ALL_BF),
                )),
                Rc::new(RegexTree::Concat(
                    Rc::new(RegexTree::range(tuple_x[0] + 1..=tuple_y[0] - 1)),
                    encode_same_level_expanded(level - 1, &ALL_80, &ALL_BF),
                )),
            )),
            Rc::new(RegexTree::Concat(
                Rc::new(RegexTree::single(tuple_y[0])),
                encode_same_level_expanded(level - 1, &ALL_80, &tuple_y[1..]),
            )),
        ))
    }
}

fn encode_le_expanded(level: usize, fst_bound: u8, tuple: &[u8]) -> Rc<RegexTree> {
    if level == 1 {
        return Rc::new(RegexTree::range(fst_bound..=tuple[0]));
    }
    normalize(Rc::new(RegexTree::Union(
        Rc::new(RegexTree::Concat(
            Rc::new(RegexTree::single(tuple[0])),
            encode_le_expanded(level - 1, 0x80, &tuple[1..]),
        )),
        Rc::new(RegexTree::Concat(
            Rc::new(RegexTree::range(fst_bound..=tuple[0] - 1)),
            encode_le_expanded(level - 1, 0x80, &ALL_BF),
        )),
    )))
}

fn encode_ge_expanded(level: usize, fst_bound: u8, tuple: &[u8]) -> Rc<RegexTree> {
    if level == 1 {
        return Rc::new(RegexTree::range(tuple[0]..=fst_bound));
    }
    normalize(Rc::new(RegexTree::Union(
        Rc::new(RegexTree::Concat(
            Rc::new(RegexTree::single(tuple[0])),
            encode_ge_expanded(level - 1, 0xBF, &tuple[1..]),
        )),
        Rc::new(RegexTree::Concat(
            Rc::new(RegexTree::range(tuple[0] + 1..=fst_bound)),
            encode_ge_expanded(level - 1, 0xBF, &ALL_80),
        )),
    )))
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
    normalize(ranges.iter().skip(1).fold(ranges[0].clone(), |acc, x| {
        Rc::new(RegexTree::Union(acc, x.clone()))
    }))
}

#[cfg(test)]
mod test {
    #[test]
    fn encode_char() {
        assert_eq!(super::encode_char('a').to_string(), "'a'");
        assert_eq!(super::encode_char('b').to_string(), "'b'");
        assert_eq!(super::encode_char('æ').to_string(), r#"(0xC3 ~ 0xA6)"#);
        assert_eq!(
            super::encode_char('我').to_string(),
            r#"((0xE6 ~ 0x88) ~ 0x91)"#
        );
    }

    #[test]
    fn encode_range() {
        assert_eq!(super::encode_range('a', 'a').to_string(), "'a'");
        assert_eq!(super::encode_range('a', 'b').to_string(), r#"['a', 'b']"#);
        assert_eq!(
            super::encode_range('\u{80}', '\u{88}').to_string(),
            r#"(0xC2 ~ [0x80, 0x88])"#
        );
        assert_eq!(
            super::encode_range('\u{81}', '\u{7FA}').to_string(),
            r#"((0xC2 ~ [0x81, 0xBF]) ∪ (([0xC3, 0xDE] ~ [0x80, 0xBF]) ∪ (0xDF ~ [0x80, 0xBA])))"#
        );
        assert_eq!(
            super::encode_range('\u{800}', '\u{808}').to_string(),
            r#"(0xE0 ~ (0xA0 ~ [0x80, 0x88]))"#
        );
        assert_eq!(
            super::encode_range('\u{881}', '\u{FFA}').to_string(),
            r#"(0xE0 ~ ((0xA2 ~ [0x81, 0xBF]) ∪ (([0xA3, 0xBE] ~ [0x80, 0xBF]) ∪ (0xBF ~ [0x80, 0xBA]))))"#
        );
        assert_eq!(
            super::encode_range('\u{901}', '\u{FF00}').to_string(),
            r#"((0xE0 ~ ((0xA4 ~ [0x81, 0xBF]) ∪ ([0xA5, 0xBF] ~ [0x80, 0xBF]))) ∪ (([0xE1, 0xEE] ~ ([0x80, 0xBF] ~ [0x80, 0xBF])) ∪ (0xEF ~ (([0x80, 0xBB] ~ [0x80, 0xBF]) ∪ (0xBC ~ 0x80)))))"#
        );
        assert_eq!(
            super::encode_range('a', '\u{90}').to_string(),
            r#"(['a', '\u{7f}'] ∪ (([0xC0, 0xC1] ~ [0x80, 0xBF]) ∪ (0xC2 ~ [0x80, 0x90])))"#
        );
        assert_eq!(
            super::encode_range('a', '\u{801}').to_string(),
            r#"(['a', '\u{7f}'] ∪ (([0xC0, 0xDF] ~ [0x80, 0xBF]) ∪ (0xE0 ~ (([0x80, 0x9F] ~ [0x80, 0xBF]) ∪ (0xA0 ~ [0x80, 0x81])))))"#
        );
        assert_eq!(super::encode_range('\u{99}', '\u{2771}').to_string(), "((0xC2 ~ [0x99, 0xBF]) ∪ (([0xC3, 0xDF] ~ [0x80, 0xBF]) ∪ (([0xE0, 0xE1] ~ ([0x80, 0xBF] ~ [0x80, 0xBF])) ∪ (0xE2 ~ (([0x80, 0x9C] ~ [0x80, 0xBF]) ∪ (0x9D ~ [0x80, 0xB1]))))))")
    }
}
