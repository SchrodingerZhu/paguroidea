use core::{marker::PhantomData, ops::Range, str::Utf8Error};

#[derive(Default, Clone, Copy)]
struct Brand<'id>(PhantomData<*mut &'id ()>);

#[derive(Default, Clone, Copy)]
pub struct Source<'src, 'id>(&'src [u8], Brand<'id>);

#[derive(Default, Clone, Copy)]
pub struct Span<'src, 'id>(&'src [u8], Brand<'id>);

#[cfg(feature = "alloc")]
pub struct Utf8LineCache<'src, 'id>(
    alloc::boxed::Box<[*const u8]>,
    *const u8,
    Brand<'id>,
    PhantomData<&'src ()>,
);

impl<'src, 'id> Source<'src, 'id> {
    pub fn new<R>(input: &'src [u8], f: impl for<'a> FnOnce(Source<'src, 'a>) -> R) -> R {
        f(Self(input, Brand::default()))
    }
    pub fn as_bytes(&self) -> &'src [u8] {
        self.0
    }
    pub fn as_str(&self) -> Result<&'src str, Utf8Error> {
        core::str::from_utf8(self.0)
    }
    pub fn span(&self, range: Range<usize>) -> Span<'src, 'id> {
        Span(&self.0[range], Brand::default())
    }
    pub fn start_offset(&self, span: &Span<'src, 'id>) -> usize {
        unsafe { span.0.as_ptr().offset_from(self.0.as_ptr()) as usize }
    }
    pub fn end_offset(&self, span: &Span<'src, 'id>) -> usize {
        self.start_offset(span) + span.0.len()
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[cfg(feature = "alloc")]
    pub fn utf8_line_cache(&self) -> Result<Utf8LineCache<'src, 'id>, Utf8Error> {
        Ok(Utf8LineCache(
            self.as_str()?.lines().map(|line| line.as_ptr()).collect(),
            self.0.as_ptr().wrapping_offset(self.0.len() as isize),
            Brand::default(),
            PhantomData,
        ))
    }
}

impl<'src, 'id> Span<'src, 'id> {
    pub fn as_bytes(&self) -> &'src [u8] {
        self.0
    }
    pub fn as_str(&self) -> Result<&'src str, Utf8Error> {
        core::str::from_utf8(self.0)
    }
    pub fn start_offset(&self, source: &Source<'src, 'id>) -> usize {
        source.start_offset(self)
    }
    pub fn end_offset(&self, source: &Source<'src, 'id>) -> usize {
        source.end_offset(self)
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[cfg(feature = "alloc")]
    pub fn byte_linecol(&self, cache: &Utf8LineCache<'src, 'id>) -> (usize, usize) {
        let line = cache
            .0
            .binary_search(&self.0.as_ptr())
            .unwrap_or_else(|x| x - 1);
        let col = self.0.as_ptr() as usize - cache.0[line] as usize;
        (line, col)
    }
    #[cfg(feature = "alloc")]
    pub fn char_linecol(
        &self,
        cache: &Utf8LineCache<'src, 'id>,
    ) -> (usize, Result<usize, Utf8Error>) {
        let line = cache
            .0
            .binary_search(&self.0.as_ptr())
            .unwrap_or_else(|x| x - 1);
        let col = self.0.as_ptr() as usize - cache.0[line] as usize;
        let bytes = unsafe { core::slice::from_raw_parts(self.0.as_ptr(), col) };
        let string = match core::str::from_utf8(bytes) {
            Ok(string) => string,
            Err(e) => return (line, Err(e)),
        };
        let char_col = string.chars().count();
        (line, Ok(char_col))
    }
}

#[cfg(feature = "alloc")]
impl<'src, 'id> Utf8LineCache<'src, 'id> {
    pub fn line(&self, no: usize) -> Option<&'src str> {
        let line_start = self.0.get(no).copied()?;
        let line_end = self.0.get(no + 1).copied().unwrap_or(self.1);
        Some(unsafe {
            core::str::from_utf8_unchecked(core::slice::from_raw_parts(
                line_start,
                line_end as usize - line_start as usize,
            ))
        })
    }
    pub fn byte_linecol(&self, span: &Span<'src, 'id>) -> (usize, usize) {
        span.byte_linecol(self)
    }
    pub fn char_linecol(&self, span: &Span<'src, 'id>) -> (usize, Result<usize, Utf8Error>) {
        span.char_linecol(self)
    }
}

#[cfg(test)]
mod test {
    use super::Source;
    #[test]
    fn test_simple() {
        let src = "hello world\n123abc\n";
        Source::new(src.as_bytes(), |src| {
            let span = src.span(1..4);
            assert_eq!(span.as_str().unwrap(), "ell");
            assert_eq!(span.start_offset(&src), 1);
            assert_eq!(span.end_offset(&src), 4);
            assert_eq!(span.len(), 3);
            #[cfg(feature = "alloc")]
            {
                let cache = src.utf8_line_cache().unwrap();
                assert_eq!(cache.line(0).unwrap().trim(), "hello world");
                assert_eq!(cache.line(1).unwrap().trim(), "123abc");
                assert_eq!(cache.line(2), None);
                assert_eq!(cache.byte_linecol(&span), (0, 1));
                assert_eq!(cache.char_linecol(&span), (0, Ok(1)));
            }
        });
    }
    #[test]
    fn test_unicode() {
        let src = "你好呀\n123abc\n👋🌍";
        Source::new(src.as_bytes(), |src| {
            let span = src.span(3..9);
            assert_eq!(span.as_str().unwrap(), "好呀");
            assert_eq!(span.start_offset(&src), 3);
            assert_eq!(span.end_offset(&src), 9);
            assert_eq!(span.len(), 6);
            #[cfg(feature = "alloc")]
            {
                let cache = src.utf8_line_cache().unwrap();
                assert_eq!(cache.line(0).unwrap().trim(), "你好呀");
                assert_eq!(cache.line(1).unwrap().trim(), "123abc");
                assert_eq!(cache.line(2).unwrap().trim(), "👋🌍");
                assert_eq!(cache.line(3), None);
                assert_eq!(cache.byte_linecol(&span), (0, 3));
                assert_eq!(cache.char_linecol(&span), (0, Ok(1)));
            }
        });
    }
}
