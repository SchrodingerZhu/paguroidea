use std::fs::File;
use std::io::Read;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::rc::Rc;

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
