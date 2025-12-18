use core::iter::Iterator;

use crate::lexer::Token;
use crate::parser::ParseError;
use crate::span::Span;
use crate::span::Spanned;

pub trait FromTokens: Sized + Spanned {
    /// If this returns Ok(), then `ts` has been advanced past the parsed item.
    /// If this returns Err(), then `ts` MUST remain where it was before this function was called.
    fn from_tokens(
        ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
    ) -> Result<Self, ParseError>;
    fn from_raw_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
        let tokens = ts.map(|token| (token, Span::empty())).collect::<Vec<_>>();
        Self::from_tokens(&mut tokens.into_iter())
    }
}

impl<T> FromTokens for Box<T>
where
    T: FromTokens,
{
    fn from_tokens(
        ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
    ) -> Result<Self, ParseError> {
        let mut iter = ts.clone();
        let out = T::from_tokens(&mut iter)?;
        *ts = iter;
        Ok(Box::new(out))
    }
}

impl<T> FromTokens for Option<T>
where
    T: FromTokens,
{
    fn from_tokens(
        ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
    ) -> Result<Self, ParseError> {
        let mut iter = ts.clone();
        Ok(match T::from_tokens(&mut iter).ok() {
            Some(v) => {
                *ts = iter;
                Some(v)
            }
            None => None,
        })
    }
}

impl<T> FromTokens for Vec<T>
where
    T: FromTokens,
{
    fn from_tokens(
        ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
    ) -> Result<Self, ParseError> {
        let mut out = Self::new();
        let mut iter = ts.clone();
        while let Ok(v) = T::from_tokens(&mut iter) {
            out.push(v);
        }
        *ts = iter;
        Ok(out)
    }
}

/// We use this instead of IntoIterator b/c it's too hard to directly name the IntoIter type.
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
