use core::iter::Iterator;

use crate::span::Span;
use crate::span::Spanned;

/// A trait that represents an iterator that can be cheaply `.clone()`-ed. This is used for
/// backtracking: we clone the iterator, greedily take values from it until:
/// + we encounter a parse error, in which case we throw away the new iterator
/// + we successfully parse the current node, in which case we keep the current iterator.
trait CloneableIterator: Iterator + Clone {}

/// Creates a CloneableIterator from `Vec<(T, Span)>`, created with `lex()`, for use with the
/// parser.
fn as_cloneable<T>(tokens: &Vec<(T, Span)>) -> impl CloneableIterator<Item = (&'_ T, Span)> {
    todo!()
}`

pub trait FromTokens<Token, Error>: Sized + Spanned {
    /// If this returns Ok(), then `ts` has been advanced past the parsed item.
    /// If this returns Err(), then `ts` MUST remain where it was before this function was called.
    fn from_tokens<'a>(
        ts: &mut impl CloneableIterator<Item = (&'a Token, Span)>,
    ) -> Result<Self, Error>
    where
        Token: 'a;
    fn from_raw_tokens(ts: impl IntoIterator<Item = Token>) -> Result<Self, Error> {
        let tokens = ts
            .into_iter()
            .map(|token| (token, Span::empty()))
            .collect::<Vec<_>>();
        let mut tokens = as_cloneable(&tokens);
        Self::from_tokens(&mut tokens)
    }
}

impl<Token, Error, T> FromTokens<Token, Error> for Box<T>
where
    T: FromTokens<Token, Error>,
{
    fn from_tokens<'a>(
        ts: &mut impl CloneableIterator<Item = (&'a Token, Span)>,
    ) -> Result<Self, Error>
    where
        Token: 'a,
    {
        let mut iter = ts.clone();
        let out = T::from_tokens(&mut iter)?;
        *ts = iter;
        Ok(Box::new(out))
    }
}

impl<Token, Error, T> FromTokens<Token, Error> for Option<T>
where
    T: FromTokens<Token, Error>,
{
    fn from_tokens<'a>(
        ts: &mut impl CloneableIterator<Item = (&'a Token, Span)>,
    ) -> Result<Self, Error>
    where
        Token: 'a,
    {
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

impl<Token, Error, T> FromTokens<Token, Error> for Vec<T>
where
    T: FromTokens<Token, Error>,
{
    fn from_tokens<'a>(
        ts: &mut impl CloneableIterator<Item = (&'a Token, Span)>,
    ) -> Result<Self, Error>
    where
        Token: 'a,
    {
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
pub trait ToTokens<Token> {
    fn to_tokens(self) -> impl Iterator<Item = Token>;
}

impl<Token, T> ToTokens<Token> for Box<T>
where
    T: ToTokens<Token>,
{
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        (*self).to_tokens()
    }
}

impl<Token, T> ToTokens<Token> for Option<T>
where
    T: ToTokens<Token>,
{
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        self.into_iter().flat_map(ToTokens::to_tokens)
    }
}

impl<Token, T> ToTokens<Token> for Vec<T>
where
    T: ToTokens<Token>,
{
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        self.into_iter().flat_map(ToTokens::to_tokens)
    }
}
