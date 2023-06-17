use std::io::Write;
use tempfile::NamedTempFile;

fn generate_tokenizer<A, B, I>(name: &str, rules: I, skip: Option<&str>)
where
    I: AsRef<[(A, B)]>,
    A: AsRef<str>,
    B: AsRef<str>,
{
    let mut file = NamedTempFile::new().unwrap();
    writeln!(file.as_file(), "lexer {{").unwrap();
    for (name, rule) in rules.as_ref() {
        writeln!(file.as_file(), "{} = {};", name.as_ref(), rule.as_ref()).unwrap()
    }
    if let Some(skip) = skip {
        writeln!(file.as_file(), "skip = {skip};").unwrap()
    }
    writeln!(file.as_file(), "}}").unwrap();
    writeln!(file.as_file(), "parser tokens {{").unwrap();
    for (name, _) in rules.as_ref() {
        let lowercase = name.as_ref().to_lowercase();
        writeln!(file.as_file(), "active {lowercase} = {};", name.as_ref()).unwrap()
    }
    writeln!(
        file.as_file(),
        "active tokens = ({})*;",
        rules
            .as_ref()
            .iter()
            .map(|(name, _)| name.as_ref().to_lowercase())
            .collect::<Vec<_>>()
            .join("|")
    )
    .unwrap();
    writeln!(file.as_file(), "}}").unwrap();
    file.as_file_mut().flush().unwrap();
    pag_compiler::compile(file.path(), format!("src/generated/{name}.rs"));
}

fn main() {
    std::fs::create_dir_all("src/generated").unwrap();
    generate_tokenizer(
        "length_differential",
        [
            ("A", r#"'a'"#),
            ("AA", r#""aa""#),
            ("AAA", r#""aaa""#),
            ("AAAA", r#""aaaa""#),
            ("AAAAA", r#""aaaaa""#),
            ("MORE", r"AAAAA~ 'a'+"),
        ],
        Some(r"'\n' | '\r' | '\t' | ' '"),
    );
    generate_tokenizer(
        "common_prefix",
        {
            let mut rules = Vec::new();
            let mut current = String::new();
            for i in 'A'..='Z' {
                current.push(i);
                rules.push((current.clone(), format!("{:?}", current)));
            }
            rules
        },
        Some(r"'\n' | '\r' | '\t' | ' '"),
    );
    generate_tokenizer(
        "comment_and_string",
        [
            ("STRING", r#"'\"' ~ ( (!'\"')* | "\"\"") ~ '\"'"#),
            // (
            //     "COMMENT",
            //     r#""/*" ~ (!('\x00'..'\xff'* ~ "*/" ~ '\x00'..'\xff'*))* ~ "*/""#,
            // ),
        ],
        Some(r"'\n' | '\r' | '\t' | ' '"),
    );
}
