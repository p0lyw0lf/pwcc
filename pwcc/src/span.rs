use std::fmt::Display;
use std::ops::Deref;
use std::ops::DerefMut;

use functional::ControlFlow;
use functional::Foldable;
use functional::Functor;
use functional::RecursiveCall;
use functional::Semigroup;
use functional::TryFunctor;

/// Newtype for miette::SourceSpan that allows it to be grown. Also has a representation that's
/// easier for me to work with, despite being less safe
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SourceSpan {
    start: usize,
    /// MUST be >= self.start
    end: usize,
}

impl SourceSpan {
    pub const fn empty() -> Self {
        Self { start: 0, end: 0 }
    }
}

impl Semigroup for SourceSpan {
    fn sconcat(self, other: Self) -> Self {
        if self == SourceSpan::empty() {
            other
        } else if other == SourceSpan::empty() {
            self
        } else {
            Self {
                start: std::cmp::min(self.start, other.start),
                end: std::cmp::max(self.end, other.end),
            }
        }
    }
}

impl Display for SourceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.start, self.end)
    }
}

impl Into<miette::SourceSpan> for SourceSpan {
    fn into(self) -> miette::SourceSpan {
        (self.start, self.end - self.start).into()
    }
}

impl Into<miette::SourceSpan> for &SourceSpan {
    fn into(self) -> miette::SourceSpan {
        (*self).into()
    }
}

impl<T> From<T> for SourceSpan
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

/// Span<T> should be transparent to T for basically all trait implementations, except for `Into`
/// so that it can be used as a `#[label]` in miette diagnostics.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Span<T> {
    pub inner: T,
    pub span: SourceSpan,
}

impl<T> Into<miette::SourceSpan> for Span<T> {
    fn into(self) -> miette::SourceSpan {
        self.span.into()
    }
}

impl<T> Into<miette::SourceSpan> for &Span<T> {
    fn into(self) -> miette::SourceSpan {
        self.span.into()
    }
}

impl<T> Span<T> {
    pub fn boxed(self) -> Span<Box<T>> {
        Span {
            inner: Box::new(self.inner),
            span: self.span,
        }
    }
}

/// Helper trait so that, when constructing nodes that are surrounded by Span<> for testing, it's
/// much easier
pub trait Spanned: Sized {
    fn span(self, s: SourceSpan) -> Span<Self>;
}

impl<T> Spanned for T {
    #[inline(always)]
    fn span(self, s: SourceSpan) -> Span<Self> {
        Span {
            inner: self,
            span: s,
        }
    }
}

impl<T> Deref for Span<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Span<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T> Display for Span<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

/// First, we need to make sure that Span is transparent as a wrapper type, similar to how Vec,
/// Box, and Option behave.
impl<T, Output> Functor<Output> for Span<T>
where
    T: Functor<Output>,
{
    type Input = T::Input;
    type Mapped = Span<T::Mapped>;

    #[inline(always)]
    fn fmap_impl(
        self,
        f: &mut impl FnMut(Self::Input) -> Output,
        how: RecursiveCall,
    ) -> Self::Mapped {
        Span {
            inner: self.inner.fmap_impl(f, how),
            span: self.span,
        }
    }
}

impl<T, Output> TryFunctor<Output> for Span<T>
where
    T: TryFunctor<Output>,
{
    #[inline(always)]
    fn try_fmap_impl<E: Semigroup + ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Output, E>,
        how: RecursiveCall,
    ) -> Result<Self::Mapped, E> {
        Ok(Span {
            inner: self.inner.try_fmap_impl(f, how)?,
            span: self.span,
        })
    }
}

impl<T, A> Foldable<A> for Span<T>
where
    T: Foldable<A>,
{
    #[inline(always)]
    fn foldl_impl<'s, B>(
        &'s self,
        f: &mut impl FnMut(B, &'s A) -> B,
        acc: B,
        how: RecursiveCall,
    ) -> B
    where
        A: 's,
    {
        self.inner.foldl_impl(f, acc, how)
    }
}
