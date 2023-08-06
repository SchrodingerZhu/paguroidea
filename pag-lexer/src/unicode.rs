use std::ops::RangeInclusive;

use crate::intervals;
use crate::intervals::Intervals;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnicodeState {
    /// Unicode = ( Ascii
    ///           | 0x80..0x7FF
    ///           | 0x800..0xCFFF
    ///           | 0xD000..0xD7FF
    ///           | 0xE000..0xFFFF
    ///           | 0x10000..0x3FFFF
    ///           | 0x40000..0xFFFFF
    ///           | 0x100000..0x10FFFF
    ///           )*
    Accept,
    // Multiple of 0b10_000000, 0b10_111111
    UnicodeTail(u8),
    // special range followed by multiple of tails
    UnicodeMiddle(RangeInclusive<u8>, u8),
    Reject,
}

impl UnicodeState {
    pub fn next(&self, x: u8) -> Self {
        use UnicodeState::*;
        match (self, x) {
            // ascii range
            (Accept, 0x00..=0x7F) => Accept,
            // 0x80 - 0x7FF
            (Accept, 0b110_00000..=0b110_11111) => UnicodeTail(1),
            // 0x800 - 0xCFFF
            (Accept, 0b1110_0000..=0b1110_1100) => UnicodeTail(2),
            // 0xD000 - 0xD7FF
            (Accept, 0b1110_1101) => UnicodeMiddle(0b10_000000..=0b10_011111, 1),
            // 0xE000 - 0xFFFF
            (Accept, 0b1110_1110..=0b1110_1111) => UnicodeTail(2),
            // 0x10000 - 0x3FFFF
            (Accept, 0b11110_000) => UnicodeMiddle(0b10_000000..=0b10_011111, 2),
            // 0x40000 - 0xFFFFF
            (Accept, 0b11110_001..=0b11110_011) => UnicodeTail(3),
            // 0x100000 - 0x10FFFF
            (Accept, 0b11110_100) => UnicodeMiddle(0b10_000000..=0b10_001111, 2),
            (UnicodeTail(1), 0b10_000000..=0b10_111111) => Accept,
            (UnicodeTail(x), 0b10_000000..=0b10_111111) => UnicodeTail(x.wrapping_sub(1)),
            (UnicodeMiddle(range, n), x) if range.contains(&x) => UnicodeTail(*n),
            _ => Reject,
        }
    }
    pub fn is_accept(&self) -> bool {
        matches!(self, UnicodeState::Accept)
    }
    pub fn is_reject(&self) -> bool {
        matches!(self, UnicodeState::Reject)
    }
    pub fn accept_sequence(&self, seq: &[u8]) -> bool {
        seq.iter()
            .fold(self.clone(), |state, &x| state.next(x))
            .is_accept()
    }
    pub fn congruence_classes(&self) -> Box<[Intervals]> {
        use UnicodeState::*;
        match self {
            Accept => [
                // => Accept
                intervals!((0x00, 0x7F)),
                // => Tail 1
                intervals!((0b110_00000, 0b110_11111)),
                // => Tail 2
                intervals!((0b1110_0000, 0b1110_1100), (0b1110_1110, 0b1110_1111)),
                // => UnicodeMiddle(0b10_000000..=0b10_011111, 1)
                intervals!((0b1110_1101, 0b1110_1101)),
                // => UnicodeMiddle(0b10_000000..=0b10_011111, 2)
                intervals!((0b11110_000, 0b11110_000)),
                // => Tail 3
                intervals!((0b11110_001, 0b11110_011)),
                // => UnicodeMiddle(0b10_000000..=0b10_001111, 2)
                intervals!((0b11110_100, 0b11110_100)),
                // => Reject
                intervals!((0b10_000000, 0b10_111111), (0b11110_101, u8::MAX)),
            ]
            .into(),
            UnicodeTail(_) => [
                // => Progress
                intervals!((0b10_000000, 0b10_111111)),
                // => Reject
                intervals!((0b00_000000, 0b01_111111), (0b11_000000, u8::MAX)),
            ]
            .into(),
            UnicodeMiddle(range, _) => [
                // => Progress
                intervals!((*range.start(), *range.end())),
                // => Reject
                intervals!((u8::MIN, *range.start() - 1), (range.end() + 1, u8::MAX)),
            ]
            .into(),
            Reject => [intervals!((u8::MIN, u8::MAX))].into(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::intervals;

    use super::UnicodeState;

    #[test]
    fn congruence_classes_form_valid_partition() {
        for state in [
            UnicodeState::Accept,
            UnicodeState::UnicodeTail(1),
            UnicodeState::UnicodeTail(2),
            UnicodeState::UnicodeTail(3),
            UnicodeState::UnicodeMiddle(0b10_000000..=0b10_011111, 1),
            UnicodeState::UnicodeMiddle(0b10_000000..=0b10_011111, 2),
            UnicodeState::UnicodeMiddle(0b10_000000..=0b10_001111, 2),
            UnicodeState::Reject,
        ] {
            let classes = state.congruence_classes();
            let mut union = classes[0].clone();
            for class in classes[1..].iter() {
                union = union.union(class);
            }
            for i in classes.iter() {
                for j in classes.iter() {
                    if i != j {
                        assert_eq!(i.intersection(j), None);
                        let next_i = state.next(i.representative());
                        let next_j = state.next(j.representative());
                        assert_ne!(next_i, next_j);
                    }
                }
            }
            assert_eq!(union, intervals!((u8::MIN, u8::MAX)));
        }
    }

    #[test]
    fn valid_next_state() {
        let all_possible_states = [
            UnicodeState::Accept,
            UnicodeState::UnicodeTail(1),
            UnicodeState::UnicodeTail(2),
            UnicodeState::UnicodeTail(3),
            UnicodeState::UnicodeMiddle(0b10_000000..=0b10_011111, 1),
            UnicodeState::UnicodeMiddle(0b10_000000..=0b10_011111, 2),
            UnicodeState::UnicodeMiddle(0b10_000000..=0b10_001111, 2),
            UnicodeState::Reject,
        ];
        for state in all_possible_states.iter() {
            for byte in u8::MIN..=u8::MAX {
                let next_state = state.next(byte);
                assert!(
                    all_possible_states.contains(&next_state),
                    "next state of {:?} with byte {} is {:?}",
                    state,
                    byte,
                    next_state
                );
            }
        }
    }

    #[test]
    fn accept_unicode_sequences() {
        let sample_ascii = b"Hello, world!";
        assert!(UnicodeState::Accept.accept_sequence(sample_ascii));

        let sample_chinese = "你好，世界！".as_bytes();
        assert!(UnicodeState::Accept.accept_sequence(sample_chinese));

        let sample_emoji = "👋🌍".as_bytes();
        assert!(UnicodeState::Accept.accept_sequence(sample_emoji));

        let sample_greek = "Γειά σου Κόσμε".as_bytes();
        assert!(UnicodeState::Accept.accept_sequence(sample_greek));

        let sample_japanese = "こんにちは世界".as_bytes();
        assert!(UnicodeState::Accept.accept_sequence(sample_japanese));

        let sample_korean = "안녕하세요 세계".as_bytes();
        assert!(UnicodeState::Accept.accept_sequence(sample_korean));

        let sample_russian = "Привет, мир!".as_bytes();
        assert!(UnicodeState::Accept.accept_sequence(sample_russian));

        let sample_arabic = "مرحبا بالعالم".as_bytes();
        assert!(UnicodeState::Accept.accept_sequence(sample_arabic));

        let sample_combined = "Hello, World!你好，世界！👋🌍Γειά σου Κόσμεこんにちは世界안녕하세요 세계Привет, мир!مرحبا بالعالم".as_bytes();
        assert!(UnicodeState::Accept.accept_sequence(sample_combined));
    }

    #[test]
    fn greek_word() {
        let sample = "κόσμε".as_bytes();
        assert!(UnicodeState::Accept.accept_sequence(sample));
    }

    #[test]
    fn lower_bounds() {
        assert!(UnicodeState::Accept.accept_sequence("\u{0000}".as_bytes()));
        assert!(UnicodeState::Accept.accept_sequence("\u{0080}".as_bytes()));
        assert!(UnicodeState::Accept.accept_sequence("\u{0800}".as_bytes()));
        assert!(UnicodeState::Accept.accept_sequence("\u{10000}".as_bytes()));
    }
    #[test]
    fn upper_bounds() {
        assert!(UnicodeState::Accept.accept_sequence("\u{007F}".as_bytes()));
        assert!(UnicodeState::Accept.accept_sequence("\u{07FF}".as_bytes()));
        assert!(UnicodeState::Accept.accept_sequence("\u{FFFF}".as_bytes()));
        assert!(UnicodeState::Accept.accept_sequence("\u{10FFFF}".as_bytes()));
    }
    #[test]
    fn countinuation_bytes() {
        assert!(!UnicodeState::Accept.accept_sequence(b"\x80"));
        assert!(!UnicodeState::Accept.accept_sequence(b"\xBF"));
        assert!(!UnicodeState::Accept.accept_sequence(b"\x80\xBF"));
        // all 64 possible continuation bytes
        let mut continuation_bytes = Vec::new();
        for i in 0x80..=0xBF {
            continuation_bytes.push(i);
        }
        assert!(!UnicodeState::Accept.accept_sequence(&continuation_bytes));
        // unicode combines with continuation bytes
        let alphabet = "Hello, World!你好，世界！👋🌍Γειά σου Κόσμεこんにちは世界안녕하세요 세계Привет, мир!مرحبا بالعالم";
        let length = alphabet.chars().count();
        for i in 0..length {
            let mut bytes = Vec::new();
            let to_insert = (0x80 + i % 64) as u8;
            let mut chars = alphabet.chars();
            for _ in 0..i {
                let char = chars.next().unwrap();
                bytes.extend(char.to_string().bytes());
            }
            bytes.push(to_insert);
            for char in chars {
                bytes.extend(char.to_string().bytes());
            }
            assert!(!UnicodeState::Accept.accept_sequence(&bytes));
        }
    }

    #[test]
    fn lonely_start_bytes() {
        for i in 0xC0..=0xDF {
            assert!(!UnicodeState::Accept.accept_sequence(&[i, b' ']));
        }
        for i in 0xE0..=0xEF {
            assert!(!UnicodeState::Accept.accept_sequence(&[b'a', i, b' ', b' ']));
        }
        for i in 0xF0..=0xF7 {
            assert!(!UnicodeState::Accept.accept_sequence(&[b'b', i, b' ', b' ', b' ']));
        }
        for i in 0xF8..=0xFB {
            assert!(!UnicodeState::Accept.accept_sequence(&[b'c', i, b' ', b' ', b' ', b' ']));
        }
        for i in 0xFC..=0xFD {
            assert!(!UnicodeState::Accept.accept_sequence(&[b'd', i, b' ', b' ', b' ', b' ', b' ']));
        }
        // valid sequences with lonely start bytes inserted
        let alphabet = "Hello, World!你好，世界！👋🌍Γειά σου Κόσμεこんにちは世界안녕하세요 세계Привет, мир!مرحبا بالعالم";
        let length = alphabet.chars().count();
        for i in 0..length {
            let mut bytes = Vec::new();
            let to_insert = (0xC0 + i % 30) as u8;
            let mut chars = alphabet.chars();
            for _ in 0..i {
                let char = chars.next().unwrap();
                bytes.extend(char.to_string().bytes());
            }
            bytes.push(to_insert);
            bytes.push(b' ');
            for char in chars {
                bytes.extend(char.to_string().bytes());
            }
            assert!(!UnicodeState::Accept.accept_sequence(&bytes));
        }
    }

    #[test]
    fn missing_continuation() {
        let alphabet = "Hello, World!你好，世界！👋🌍Γειά σου Κόσμεこんにちは世界안녕하세요 세계Привет, мир!مرحبا بالعالم";
        let length = alphabet.chars().count();
        // remove one byte from each sequence
        for i in 0..length {
            let mut bytes = Vec::new();
            let mut chars = alphabet.chars();
            for _ in 0..i {
                let char = chars.next().unwrap();
                bytes.extend(char.to_string().bytes());
            }
            let char = chars.next().unwrap();
            if char.is_ascii() {
                continue;
            }
            bytes.extend(char.to_string().bytes());
            bytes.pop();
            for char in chars {
                bytes.extend(char.to_string().bytes());
            }
            assert!(!UnicodeState::Accept.accept_sequence(&bytes));
        }
    }
}
