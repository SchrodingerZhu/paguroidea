extern crate lexical;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Pvalue<'a> {
    Number(f64),
    String(&'a str),
    Object(Vec<(&'a str, Pvalue<'a>)>),
    Bool(bool),
    Null,
    Array(Vec<Pvalue<'a>>),
}

impl<'a> fmt::Display for Pvalue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pvalue::Number(float) => write!(f, "{}", lexical::to_string(*float)),
            Pvalue::String(string) => write!(f, "\"{}\"", string),
            Pvalue::Object(obj) => {
                write!(f, "{{")?;
                if let Some(((key, value), rest)) = obj.split_first() {
                    write!(f, "\"{}\": {}", key, value)?;
                    for (key, value) in rest.iter() {
                        write!(f, ", \"{}\": {}", key, value)?
                    }
                }
                write!(f, "}}")
            }
            Pvalue::Bool(flag) => write!(f, "{}", flag),
            Pvalue::Null => write!(f, "null"),
            Pvalue::Array(array) => {
                write!(f, "[")?;
                if let Some((value, rest)) = array.split_first() {
                    write!(f, "{}", value)?;
                    for value in rest.iter() {
                        write!(f, ", {}", value)?
                    }
                }
                write!(f, "]")
            }
        }
    }
}
