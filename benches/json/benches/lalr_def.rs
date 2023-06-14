use logos::Logos;
use std::fmt;

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \r\n\t]+")]
pub enum Token<'a> {
    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("null")]
    Null,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[regex(r"-?(0|[1-9][0-9]*)((\.[0-9]+)?)([eE][+-]?[0-9]+)?")]
    Number(&'a str),

    #[regex(r#""([^\\"]|\\(["\\/bfnrt]|u[0-9a-fA-F]{4}))*""#)]
    String(&'a str),
}

impl<'a> Token<'a> {
    pub fn lalrpop_lexer(
        source: &'a str,
    ) -> impl Iterator<Item = Result<(usize, Token<'a>, usize), &'static str>> {
        Self::lexer(source)
            .spanned()
            .map(|(t, r)| Ok((r.start, t.unwrap(), r.end)))
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pvalue<'a> {
    Number(&'a str),
    String(&'a str),
    Object(Vec<(&'a str, Pvalue<'a>)>),
    Bool(bool),
    Null,
    Array(Vec<Pvalue<'a>>),
}

impl<'a> fmt::Display for Pvalue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pvalue::Number(number) => write!(f, "{number}"),
            Pvalue::String(string) => write!(f, "\"{string}\""),
            Pvalue::Object(object) => {
                let iter = object.iter().map(|(k, v)| format!("\"{k}\": {v}"));
                write!(f, "{{{}}}", iter.collect::<Vec<_>>().join(", "))
            }
            Pvalue::Bool(flag) => write!(f, "{flag}"),
            Pvalue::Null => write!(f, "null"),
            Pvalue::Array(array) => {
                let iter = array.iter().map(|v| v.to_string());
                write!(f, "[{}]", iter.collect::<Vec<_>>().join(", "))
            }
        }
    }
}
