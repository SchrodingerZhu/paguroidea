use pag_lexer::regex_tree::RegexTree;
use std::rc::Rc;

pub fn encode_char(x: char) -> RegexTree {
    let c = x as u32;
    match c {
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
    }
}
