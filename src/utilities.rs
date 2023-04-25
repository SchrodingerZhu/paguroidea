use std::path::PathBuf;
use pest::{LinesSpan, Span};

#[derive(Debug, Clone)]
struct SourceCode<'a> {
    src: &'a str,
    path: Option<PathBuf>,
    line_indices: Vec<Span<'a>>,
}

fn calculate_line_indices(src: &str) -> Vec<Span> {
    unsafe {
        Span::new(src, 0, src.len())
            .unwrap_unchecked()
            .lines_span()
            .map(|x| Span::new(src, x.start(), x.end()).unwrap_unchecked())
            .collect()
    }
}

impl<'a> SourceCode<'a> {
    pub fn new(src: &'a str, path: Option<PathBuf>) -> Self {
        Self {
            src,
            path,
            line_indices: calculate_line_indices(src),
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
     fn test() {
        let src = "hello\nworld\n";
        let src_code = super::SourceCode::new(src, None);
        println!("{:?}", src_code.line_indices);
    }
}