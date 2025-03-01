use core::iter::Iterator;

use crate::lexer::SpanToken;
use crate::lexer::Token;
use crate::parser::ParseError;

pub trait FromTokens: Sized {
    fn from_tokens(ts: &mut (impl Iterator<Item = SpanToken> + Clone)) -> Result<Self, ParseError>;
    fn from_raw_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
        let tokens = ts
            .map(|token| SpanToken {
                token,
                span: (0, 0).into(),
            })
            .collect::<Vec<_>>();
        Self::from_tokens(&mut tokens.into_iter())
    }
}

impl<T> FromTokens for Box<T>
where
    T: FromTokens,
{
    fn from_tokens(ts: &mut (impl Iterator<Item = SpanToken> + Clone)) -> Result<Self, ParseError> {
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
    fn from_tokens(ts: &mut (impl Iterator<Item = SpanToken> + Clone)) -> Result<Self, ParseError> {
        let mut iter = ts.clone();
        let out = T::from_tokens(&mut iter).ok();
        if out.is_some() {
            *ts = iter;
        }
        Ok(out)
    }
}

impl<T> FromTokens for Vec<T>
where
    T: FromTokens,
{
    fn from_tokens(ts: &mut (impl Iterator<Item = SpanToken> + Clone)) -> Result<Self, ParseError> {
        let mut out = Self::new();
        let mut iter = ts.clone();
        while let Ok(subnode) = T::from_tokens(&mut iter) {
            out.push(subnode);
        }
        *ts = iter;
        Ok(out)
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
