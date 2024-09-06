use core::convert::From;
use core::fmt::Debug;
use core::iter::Iterator;

use paste::paste;

use crate::lexer::Token;

macro_rules! expect_token {
    ($ts:ident, $token:ident$(($pat:pat) : $ty:ty)?) => {
        {
            (match $ts.next() {
                None => None,
                Some(Token::$token$((v @ $pat))?) => Some(($(<$ty>::from(v))?)),
                Some(t) => {
                    eprintln!("expected {}, got {t:?} instead", stringify!($token));
                    None
                }
            })?
        }
    };
}

trait FromTokens: Sized {
    fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Option<Self>;
}

trait ToTokens {
    fn to_tokens(self, emit: &mut impl FnMut(Token));
}

macro_rules! node {
    ($node:ident ( $(
        *
        $($itoken:ident)?
        $(<$subnode:ident>)?
        $({$ctoken:ident ($pat:pat) : $ty:ty})?
    )* ) ) => {
        #[derive(Debug, PartialEq, Eq)]
        pub struct $node($(
            $($subnode,)?$($ty,)?
        )*);

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Option<Self> {
                paste! {
                $(
                    $(expect_token!(ts, $itoken);)?
                    $(let [< v_ $subnode:snake >] = $subnode::from_tokens(ts)?;)?
                    $(let [< v_ $ctoken:snake >] = expect_token!(ts, $ctoken($pat): $ty);)?
                )*
                Some($node($($([< v_ $subnode:snake >],)?$([< v_ $ctoken:snake >],)?)*))
                }
            }
        }

        impl ToTokens for $node {
            fn to_tokens(self, emit: &mut impl FnMut(Token)) {
                paste! {
                let $node($($([< v_ $subnode:snake >],)?$([< v_ $ctoken:snake >],)?)*) = self;
                $(
                    $(emit(Token::$itoken);)?
                    $([< v_ $subnode:snake >].to_tokens(emit);)?
                    $(emit(Token::$ctoken([< v_ $ctoken:snake >].into()));)?
                )*
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

pub fn parse<TS>(tokens: TS) -> Option<Program>
where
    TS: IntoIterator<Item = Token>,
    TS::IntoIter: Clone,
{
    let mut iter = tokens.into_iter();
    Program::from_tokens(&mut iter).filter(|_| {
        if let Some(t) = iter.next() {
            eprintln!("expected end of stream, got {t:?} instead");
            return false;
        }
        true
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;

    fn assert_forwards<T: FromTokens + Debug + PartialEq>(tokens: &[Token], expected: &T) {
        let actual = T::from_tokens(&mut Vec::from(tokens).into_iter());
        assert_eq!(Some(expected), actual.as_ref());
    }

    fn assert_backwards(tree: impl ToTokens + Debug + PartialEq, expected: &[Token]) {
        let mut actual = Vec::<Token>::new();
        tree.to_tokens(&mut |t| actual.push(t));
        assert_eq!(expected, actual);
    }

    fn assert_convertible(tokens: &[Token], tree: impl ToTokens + FromTokens + Debug + PartialEq) {
        assert_forwards(tokens, &tree);
        assert_backwards(tree, tokens);
    }

    #[test]
    fn int() {
        assert_convertible(&[Constant(2)], Int(2))
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
