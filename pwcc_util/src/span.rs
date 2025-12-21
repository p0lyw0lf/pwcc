use std::fmt::Display;

use functional::Semigroup;

/// Newtype for miette::SourceSpan that allows it to be grown. Also has a representation that's
/// easier for me to work with, despite being less safe
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Span {
    start: usize,
    /// MUST be >= self.start
    end: usize,
}

impl Span {
    pub const fn empty() -> Self {
        Self { start: 0, end: 0 }
    }
}

impl Semigroup for Span {
    fn sconcat(&mut self, other: Self) {
        if *self == Span::empty() {
            *self = other;
        } else if other == Span::empty() {
            // Do nothing.
        } else {
            self.start = std::cmp::min(self.start, other.start);
            self.end = std::cmp::max(self.end, other.end);
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.start, self.end)
    }
}

#[allow(clippy::from_over_into)]
impl Into<miette::SourceSpan> for Span {
    fn into(self) -> miette::SourceSpan {
        (self.start, self.end - self.start).into()
    }
}

#[allow(clippy::from_over_into)]
impl Into<miette::SourceSpan> for &Span {
    fn into(self) -> miette::SourceSpan {
        (*self).into()
    }
}

impl<T> From<T> for Span
where
    miette::SourceSpan: From<T>,
{
    fn from(value: T) -> Self {
        let s = miette::SourceSpan::from(value);
        Self {
            start: s.offset(),
            end: s.offset() + s.len(),
        }
    }
}

/// Trait to extract the Span from a given node, without needing to go thru an `Into`
/// implementation.
pub trait Spanned {
    fn span(&self) -> Span;
}

// Re-export the derive macro for this trait.
pub use pwcc_util_macros::Spanned;

impl<T> Spanned for (T, Span) {
    fn span(&self) -> Span {
        self.1
    }
}

impl<T> Spanned for Box<T>
where
    T: Spanned,
{
    fn span(&self) -> Span {
        (**self).span()
    }
}

impl<T> Spanned for Option<T>
where
    T: Spanned,
{
    fn span(&self) -> Span {
        match self {
            Some(v) => v.span(),
            None => Span::empty(),
        }
    }
}

impl<T> Spanned for Vec<T>
where
    T: Spanned,
{
    fn span(&self) -> Span {
        let mut out = Span::empty();
        for v in self.iter() {
            out.sconcat(v.span());
        }
        out
    }
}
