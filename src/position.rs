use std::fmt::Display;

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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    // TODO: Add filename?
    /// Starting position (inclusive)
    pub start: BytePos,
    /// Ending position (exclusive)
    pub end: BytePos,
}

impl Span {
    pub const fn new(start: BytePos, end: BytePos) -> Self {
        Span { start, end }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Find a way to print the actual position in the file rather than byte pos
        write!(f, "{}:{}", self.start, self.end)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct WithSpan<T> {
    pub value: T,
    pub span: Span,
}

impl<T> WithSpan<T> {
    pub const fn new(value: T, span: Span) -> Self {
        WithSpan { value, span }
    }
}
