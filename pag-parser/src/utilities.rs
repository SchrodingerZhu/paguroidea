// Copyright (c) 2023 Paguroidea Developpers
//
// Licensed under the Apache License, Version 2.0
// <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
// license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. All files in the project carrying such notice may not be copied,
// modified, or distributed except according to those terms.

use std::fs::File;
use std::io::Read;
use std::ops::Range;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialOrd, Ord)]
pub struct Symbol<'a>(&'a str);

impl<'a> Symbol<'a> {
    pub fn new(data: &'a str) -> Self {
        Self(data)
    }
    pub fn name(&self) -> &'a str {
        self.0
    }
}

impl<'a> std::hash::Hash for Symbol<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
        self.0.len().hash(state);
    }
}

fn is_ascii_ident_body(x: char) -> bool {
    x.is_ascii_alphanumeric() || x == '_'
}

fn is_ascii_ident_head(x: char) -> bool {
    x.is_ascii_alphabetic() || x == '_'
}

fn is_ascii_ident(x: &str) -> bool {
    !x.is_empty()
        && is_ascii_ident_head(unsafe { x.chars().next().unwrap_unchecked() })
        && x[1..].chars().all(is_ascii_ident_body)
}

impl<'a> std::fmt::Display for Symbol<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if is_ascii_ident(self.0) {
            write!(f, "{}", self.0)
        } else {
            write!(f, "S{:X}_{}", self.0.as_ptr() as usize, self.0.len())
        }
    }
}

impl<'a, 'b> PartialEq<Symbol<'b>> for Symbol<'a> {
    fn eq(&self, other: &Symbol<'b>) -> bool {
        self.0.as_ptr().eq(&other.0.as_ptr()) && self.0.len() == other.0.len()
    }
}

impl<'a> Eq for Symbol<'a> {}

#[derive(Debug, Clone)]
pub struct SourceCode {
    pub src: String,
    pub path: Option<PathBuf>,
    line_indices: Vec<Range<usize>>,
}

#[derive(Debug, Clone)]
pub struct SrcSpan<'a> {
    src_code: &'a SourceCode,
    range: Range<usize>,
}

fn calculate_line_indices(src: &str) -> Vec<Range<usize>> {
    unsafe {
        pest::Span::new(src, 0, src.len())
            .unwrap_unchecked()
            .lines_span()
            .map(|x| x.start()..x.end())
            .collect()
    }
}

impl SourceCode {
    pub fn new(src: String, path: Option<PathBuf>) -> Self {
        let line_indices = calculate_line_indices(&src);
        Self {
            src,
            path,
            line_indices,
        }
    }
    pub fn load_file<P: AsRef<Path>>(path: P) -> Result<Self, std::io::Error> {
        let mut src = String::new();
        File::open(path.as_ref())?.read_to_string(&mut src)?;
        Ok(Self::new(src, Some(path.as_ref().to_path_buf())))
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        let src = "hello\nworld\n".to_string();
        let src_code = super::SourceCode::new(src, None);
        println!("{:?}", src_code.line_indices);
    }
}
