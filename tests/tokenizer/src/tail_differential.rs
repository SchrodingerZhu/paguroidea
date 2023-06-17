use crate::generated::tail_differential::Tag;
#[allow(unused_imports)]
use rand::{Rng, RngCore};

#[allow(dead_code)]
fn random_generate<G: Rng>(gen: &mut G, length: usize) -> (Vec<Tag>, String) {
    let mut buffer = String::new();
    let mut tags = Vec::new();
    for _ in 0..length {
        match gen.next_u64() % 4 {
            0 => {
                //ab(c*d)?
                buffer.push_str("ab");
                if gen.next_u64() % 2 == 0 {
                    for _ in 0..gen.next_u64() % 129 {
                        buffer.push('c');
                    }
                    buffer.push('d');
                }
                tags.push(Tag::abcd);
            }
            1 => {
                // abc*e
                buffer.push_str("ab");
                for _ in 0..gen.next_u64() % 129 {
                    buffer.push('c');
                }
                buffer.push('e');
                tags.push(Tag::abce);
            }
            2 => {
                //abc*dd+
                buffer.push_str("ab");
                for _ in 0..gen.next_u64() % 129 {
                    buffer.push('c');
                }
                for _ in 0..gen.next_u64() % 129 + 2 {
                    buffer.push('d');
                }
                tags.push(Tag::abcdm);
            }
            _ => {
                // c+
                for _ in 0..gen.next_u64() % 129 + 1 {
                    buffer.push('c');
                }
                tags.push(Tag::cs);
            }
        }
    }
    (tags, buffer)
}

#[test]
fn random_tail_differential_test() {
    let mut gen = rand::thread_rng();
    for _ in 0..1000 {
        let length = gen.next_u64() as usize % 1000 + 100;
        let (mut tags, buffer) = random_generate(&mut gen, length);
        // deduplicate only for cs
        tags.dedup_by(|a, b| *a == Tag::cs && *b == Tag::cs);
        let trimmed = buffer.trim();
        let tree = crate::generated::tail_differential::parse(trimmed).unwrap();
        assert_eq!(tree.len(), trimmed.len());
        let tokens = tree.children().iter().map(|x| x.tag()).collect::<Vec<_>>();
        assert_eq!(tokens, tags);
    }
}
