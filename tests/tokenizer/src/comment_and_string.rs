use crate::generated::comment_and_string::Tag;
use rand::distributions::Uniform;

#[allow(unused_imports)]
use rand::{Rng, RngCore};

#[allow(dead_code)]
fn generate_random_string<G: Rng>(gen: &mut G, length: usize, buffer: &mut String) {
    buffer.push('"');
    let dist = Uniform::<u8>::new_inclusive(u8::MIN, u8::MAX);
    for _ in 0..length {
        let target = gen.sample(dist);
        buffer.push(target as char);
        if target == b'"' {
            buffer.push('"');
        }
    }
    buffer.push('"');
}

#[allow(dead_code)]
fn generate_random_comment<G: Rng>(gen: &mut G, length: usize, buffer: &mut String) {
    buffer.push_str("/*");
    let dist = Uniform::<u8>::new_inclusive(u8::MIN, u8::MAX);
    let mut last_is_star = false;
    for _ in 0..length {
        let mut target = gen.sample(dist);
        while last_is_star && target == b'/' {
            target = gen.sample(dist);
        }
        last_is_star = target == b'*';
        buffer.push(target as char);
    }
    buffer.push_str("*/");
}

#[allow(dead_code)]
fn random_generate<G: Rng>(gen: &mut G, length: usize) -> (Vec<Tag>, String) {
    let mut buffer = String::new();
    let mut tags = Vec::new();
    for _ in 0..length {
        let inner_length = 4;
        if gen.next_u64() % 2 == 0 {
            // generate_random_comment(gen, inner_length, &mut buffer);
            // buffer.push('\n');
            // tags.push(Tag::comment);
        } else {
            generate_random_string(gen, inner_length, &mut buffer);
            buffer.push('\n');
            tags.push(Tag::string);
        }
    }
    (tags, buffer)
}

#[test]
fn random_comment_and_string_test() {
    let x = crate::generated::comment_and_string::parse("abcccc").unwrap();
    println!("{}", x.as_slice());
}
