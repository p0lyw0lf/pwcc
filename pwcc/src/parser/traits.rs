use core::iter::Iterator;

use functional::Semigroup;

use crate::lexer::Token;
use crate::parser::ParseError;
use crate::span::SourceSpan;
use crate::span::Span;

pub trait FromTokens: Sized {
    fn from_tokens(
        ts: &mut (impl Iterator<Item = Span<Token>> + Clone),
    ) -> Result<Span<Self>, ParseError>;
    fn from_raw_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
        let tokens = ts
            .map(|token| Span {
                inner: token,
                span: (0, 0).into(),
            })
            .collect::<Vec<_>>();
        Self::from_tokens(&mut tokens.into_iter()).map(|span| span.inner)
    }
}

impl<T> FromTokens for Box<T>
where
    T: FromTokens,
{
    fn from_tokens(
        ts: &mut (impl Iterator<Item = Span<Token>> + Clone),
    ) -> Result<Span<Self>, ParseError> {
        let mut iter = ts.clone();
        let Span { inner, span } = T::from_tokens(&mut iter)?;
        *ts = iter;
        Ok(Span {
            inner: Box::new(inner),
            span,
        })
    }
}

impl<T> FromTokens for Option<T>
where
    T: FromTokens,
{
    fn from_tokens(
        ts: &mut (impl Iterator<Item = Span<Token>> + Clone),
    ) -> Result<Span<Self>, ParseError> {
        let mut iter = ts.clone();
        Ok(match T::from_tokens(&mut iter).ok() {
            Some(Span { inner, span }) => {
                *ts = iter;
                Span {
                    inner: Some(inner),
                    span,
                }
            }
            None => Span {
                inner: None,
                span: SourceSpan::empty(),
            },
        })
    }
}

impl<T> FromTokens for Vec<T>
where
    T: FromTokens,
{
    fn from_tokens(
        ts: &mut (impl Iterator<Item = Span<Token>> + Clone),
    ) -> Result<Span<Self>, ParseError> {
        let mut out = Self::new();
        let mut overall_span = SourceSpan::empty();
        let mut iter = ts.clone();
        while let Ok(Span { inner, span }) = T::from_tokens(&mut iter) {
            out.push(inner);
            overall_span = overall_span.sconcat(span);
        }
        *ts = iter;
        Ok(Span {
            inner: out,
            span: overall_span,
        })
    }
}

// IntoIterator is too hard b/c can't name the IntoIter type
pub trait ToTokens {
    fn to_tokens(self) -> impl Iterator<Item = Token>;
}

impl<T> ToTokens for Box<T>
where
    T: ToTokens,
{
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        (*self).to_tokens()
    }
}

impl<T> ToTokens for Option<T>
where
    T: ToTokens,
{
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        self.into_iter().flat_map(ToTokens::to_tokens)
    }
}

impl<T> ToTokens for Vec<T>
where
    T: ToTokens,
{
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        self.into_iter().flat_map(ToTokens::to_tokens)
    }
}
