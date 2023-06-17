use crate::generated::common_prefix::Tag;
#[allow(unused_imports)]
use rand::{Rng, RngCore};

#[allow(dead_code)]
const TABLE: &[(Tag, &str)] = &[
    (Tag::a, "A"),
    (Tag::ab, "AB"),
    (Tag::abc, "ABC"),
    (Tag::abcd, "ABCD"),
    (Tag::abcde, "ABCDE"),
    (Tag::abcdef, "ABCDEF"),
    (Tag::abcdefg, "ABCDEFG"),
    (Tag::abcdefgh, "ABCDEFGH"),
    (Tag::abcdefghi, "ABCDEFGHI"),
    (Tag::abcdefghij, "ABCDEFGHIJ"),
    (Tag::abcdefghijk, "ABCDEFGHIJK"),
    (Tag::abcdefghijkl, "ABCDEFGHIJKL"),
    (Tag::abcdefghijklm, "ABCDEFGHIJKLM"),
    (Tag::abcdefghijklmn, "ABCDEFGHIJKLMN"),
    (Tag::abcdefghijklmno, "ABCDEFGHIJKLMNO"),
    (Tag::abcdefghijklmnop, "ABCDEFGHIJKLMNOP"),
    (Tag::abcdefghijklmnopq, "ABCDEFGHIJKLMNOPQ"),
    (Tag::abcdefghijklmnopqr, "ABCDEFGHIJKLMNOPQR"),
    (Tag::abcdefghijklmnopqrs, "ABCDEFGHIJKLMNOPQRS"),
    (Tag::abcdefghijklmnopqrst, "ABCDEFGHIJKLMNOPQRST"),
    (Tag::abcdefghijklmnopqrstu, "ABCDEFGHIJKLMNOPQRSTU"),
    (Tag::abcdefghijklmnopqrstuv, "ABCDEFGHIJKLMNOPQRSTUV"),
    (Tag::abcdefghijklmnopqrstuvw, "ABCDEFGHIJKLMNOPQRSTUVW"),
    (Tag::abcdefghijklmnopqrstuvwx, "ABCDEFGHIJKLMNOPQRSTUVWX"),
    (Tag::abcdefghijklmnopqrstuvwxy, "ABCDEFGHIJKLMNOPQRSTUVWXY"),
    (
        Tag::abcdefghijklmnopqrstuvwxyz,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    ),
];

#[allow(dead_code)]
fn random_generate<G: Rng>(gen: &mut G, length: usize) -> (Vec<Tag>, String) {
    let mut buffer = String::new();
    let mut tags = Vec::new();
    for _ in 0..length {
        let (tag, s) = TABLE[gen.next_u64() as usize % TABLE.len()];
        buffer.push_str(s);
        buffer.push(' ');
        tags.push(tag);
    }
    (tags, buffer)
}

#[test]
fn random_common_prefix_test() {
    let mut gen = rand::thread_rng();
    for _ in 0..1000 {
        let length = gen.next_u64() as usize % 1000 + 100;
        let (tags, buffer) = random_generate(&mut gen, length);
        let trimmed = buffer.trim();
        let tree = crate::generated::common_prefix::parse(trimmed).unwrap();
        assert_eq!(tree.len(), trimmed.len());
        let tokens = tree
            .children()
            .iter()
            .map(|x| x.tag().clone())
            .collect::<Vec<_>>();
        assert_eq!(tokens, tags);
    }
}
