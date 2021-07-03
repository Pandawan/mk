use std::{cmp, fmt::Display};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(usize);

impl BytePos {
    pub fn shift(self, ch: char) -> Self {
        Self(self.0 + ch.len_utf8())
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
        Self { start, end }
    }

    #[cfg(test)]
    pub const fn new_unchecked(start: usize, end: usize) -> Self {
        Self {
            start: BytePos(start),
            end: BytePos(end),
        }
    }

    pub fn union(a: Self, b: Self) -> Self {
        Self::new(cmp::min(a.start, b.start), cmp::max(a.end, b.end))
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Find a way to print the actual position in the file rather than byte pos
        // This may mean removing Span's Display and instead handling that in the Display of ParseError
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

    pub const fn empty(value: T) -> Self {
        WithSpan {
            value,
            span: Span::new(BytePos(0), BytePos(0)),
        }
    }

    pub fn with_source(&self, src: &str) -> String {
        src.chars()
            .skip(self.span.start.0)
            .take(self.span.end.0 - self.span.start.0)
            .collect::<String>()
    }
}
