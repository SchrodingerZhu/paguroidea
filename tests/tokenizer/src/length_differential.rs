use crate::generated::length_differential::Tag;
#[allow(unused_imports)]
use rand::{Rng, RngCore};

#[allow(dead_code)]
fn random_generate<G: Rng>(gen: &mut G, length: usize) -> (Vec<Tag>, String) {
    let mut buffer = String::new();
    let mut tags = Vec::new();
    for _ in 0..length {
        match gen.next_u64() % 6 {
            0 => {
                buffer.push_str("a ");
                tags.push(Tag::a);
            }
            1 => {
                buffer.push_str("aa ");
                tags.push(Tag::aa);
            }
            2 => {
                buffer.push_str("aaa ");
                tags.push(Tag::aaa);
            }
            3 => {
                buffer.push_str("aaaa ");
                tags.push(Tag::aaaa);
            }
            4 => {
                buffer.push_str("aaaaa ");
                tags.push(Tag::aaaaa);
            }
            _ => {
                buffer.push_str("a".repeat(6 + gen.next_u64() as usize % 128).as_str());
                buffer.push(' ');
                tags.push(Tag::more);
            }
        }
    }
    (tags, buffer)
}

#[test]
fn random_length_differential_test() {
    let mut gen = rand::thread_rng();
    for _ in 0..1000 {
        let length = gen.next_u64() as usize % 1000 + 100;
        let (tags, buffer) = random_generate(&mut gen, length);
        let trimmed = buffer.trim();
        let tree = crate::generated::length_differential::parse(trimmed).unwrap();
        assert_eq!(tree.len(), trimmed.len());
        let tokens = tree
            .children()
            .iter()
            .map(|x| x.tag().clone())
            .collect::<Vec<_>>();
        assert_eq!(tokens, tags);
    }
}
