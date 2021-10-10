use std::fmt::Display;

/// A location somewhere in the source code
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct BytePos(usize);

impl BytePos {
    pub fn new(pos: usize) -> Self {
        BytePos(pos)
    }

    pub fn shift(self, ch: char) -> Self {
        BytePos(self.0 + ch.len_utf8())
    }
}

impl Display for BytePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A subsection of the source code
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    start: BytePos,
    end: BytePos,
    // TODO: Keep track of source
}

impl Span {
    pub fn new(start: BytePos, end: BytePos) -> Span {
        Span { start, end }
    }

    pub fn empty() -> Span {
        Span {
            start: BytePos(0),
            end: BytePos(0),
        }
    }

    /// Convert the given span to the "(at 1:1)" format
    pub fn at_str(&self) -> String {
        format!("(at {})", self)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WithSpan<T> {
    pub value: T,
    pub span: Span,
}

impl<T> WithSpan<T> {
    pub fn new(value: T, span: Span) -> WithSpan<T> {
        WithSpan { value, span }
    }
}

impl<T> WithSpan<T>
where
    T: Display,
{
    /// Convert the given WithSpan to the "value (at 1:1)" format
    /// See Span::at_str() for detail
    pub fn at_str(&self) -> String {
        format!("{} {}", self.value, self.span.at_str())
    }
}
