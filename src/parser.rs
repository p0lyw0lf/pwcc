use core::convert::From;
use core::fmt::Debug;
use core::iter::Iterator;

use paste::paste;

use crate::lexer::Token;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ParseError {
    UnexpectedToken { expected: Token, actual: Token },
    MissingToken { expected: Token },
    ExtraToken { actual: Token },
}

macro_rules! expect_token {
    ($ts:ident, $token:ident$(($pat:pat) : $ty:ty)?) => {
        {
            use ParseError::*;
            use Token::*;
            (match $ts.next() {
                None => Err(MissingToken {
                    expected: $token$((<$ty as Default>::default().into()))?,
                }),
                Some(Token::$token$((v @ $pat))?) => Ok(($(<$ty>::from(v))?)),
                Some(t) => Err(UnexpectedToken {
                    expected: $token$((<$ty as Default>::default().into()))?,
                    actual: t,
                }),
            })?
        }
    };
}

pub trait FromTokens: Sized {
    fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError>;
}

// IntoIterator is too hard b/c can't name the IntoIter type
pub trait ToTokens {
    fn to_tokens(self) -> impl Iterator<Item = Token>;
}

macro_rules! node {
    ($node:ident ( $(
        *
        $($itoken:ident)?
        $(<$subnode:ident>)?
        $({$ctoken:ident ($pat:pat) : $ty:ty})?
    )* ) ) => {
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub struct $node($(
            $($subnode,)?$($ty,)?
        )*);

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
                paste! {
                $(
                    $(expect_token!(ts, $itoken);)?
                    $(let [< v_ $subnode:snake >] = $subnode::from_tokens(ts)?;)?
                    $(let [< v_ $ctoken:snake >] = expect_token!(ts, $ctoken($pat): $ty);)?
                )*
                Ok($node($($([< v_ $subnode:snake >],)?$([< v_ $ctoken:snake >],)?)*))
                }
            }
        }

        impl ToTokens for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                paste! {
                let $node($($([< v_ $subnode:snake >],)?$([< v_ $ctoken:snake >],)?)*) = self;
                ::core::iter::empty()
                $(.chain(
                    $(::core::iter::once(Token::$itoken))?
                    $([< v_ $subnode:snake >].to_tokens())?
                    $(::core::iter::once(Token::$ctoken([< v_ $ctoken:snake >].into())))?
                ))*
                }
            }
        }
    };
}

macro_rules! nodes {
    ($($node:ident $tt:tt;)*) => {
        $(node!($node $tt);)*
    };
}

nodes! {
    Program(*<Function>);
    Function(
        *KeywordInt *<Identifier> *OpenParen *KeywordVoid *CloseParen *OpenBrace
            *<Statement>
        *CloseBrace
    );
    Statement(*KeywordReturn *<Exp> *Semicolon);
    Exp(*<Int>);
    Identifier(*{Ident(_): String});
    Int(*{Constant(_): isize});
}

pub fn parse<TS>(tokens: TS) -> Result<Program, ParseError>
where
    TS: IntoIterator<Item = Token>,
    TS::IntoIter: Clone,
{
    let mut iter = tokens.into_iter();
    Program::from_tokens(&mut iter).and_then(|p| match iter.next() {
        Some(token) => Err(ParseError::ExtraToken { actual: token }),
        None => Ok(p),
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;

    fn assert_forwards<T: FromTokens + Debug + PartialEq>(tokens: &[Token], expected: &T) {
        let actual = T::from_tokens(&mut Vec::from(tokens).into_iter());
        assert_eq!(Ok(expected), actual.as_ref());
    }

    fn assert_backwards(tree: impl ToTokens + Debug + PartialEq, expected: &[Token]) {
        let actual = tree.to_tokens().collect::<Vec<_>>();
        assert_eq!(expected, actual);
    }

    fn assert_convertible(tokens: &[Token], tree: impl ToTokens + FromTokens + Debug + PartialEq) {
        assert_forwards(tokens, &tree);
        assert_backwards(tree, tokens);
    }

    #[test]
    fn int() {
        assert_convertible(&[Constant(2)], Int(2));
    }

    #[test]
    fn ident() {
        assert_convertible(&[Ident("x".into())], Identifier("x".into()));
    }

    #[test]
    fn exp() {
        assert_convertible(&[Constant(2)], Exp(Int(2)));
    }

    #[test]
    fn statement() {
        assert_convertible(
            &[KeywordReturn, Constant(2), Semicolon],
            Statement(Exp(Int(2))),
        );
    }

    #[test]
    fn function() {
        assert_convertible(
            &[
                KeywordInt,
                Ident("main".into()),
                OpenParen,
                KeywordVoid,
                CloseParen,
                OpenBrace,
                KeywordReturn,
                Constant(2),
                Semicolon,
                CloseBrace,
            ],
            Function(Identifier("main".into()), Statement(Exp(Int(2)))),
        );
    }
}
