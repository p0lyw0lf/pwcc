use core::convert::From;
use core::fmt::Debug;
use core::iter::Iterator;

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

macro_rules! node {
    ($node:ident ( $(
        $case:ident : $(
            ~
            $($itoken:ident)?
            $(<$subnode:ident>)?
            $({$ctoken:ident ($pat:pat) : $ty:ty})?
        )*
    )|* ) ) => {
        #[derive(Debug, PartialEq, Eq)]
        pub enum $node {$(
            $case($($($subnode,)?$($ty,)?)*),
        )*}

        impl FromTokens for $node {
            fn from_tokens(iter: &mut (impl Iterator<Item = Token> + Clone)) -> Option<Self> {
                $(if let Some(($($($subnode,)?$($ctoken,)?)*)) = {
                    let mut ts = iter.clone();
                    let out = (|| -> Option<_> {
                        $(
                            $(expect_token!(ts, $itoken);)?
                            $(let $subnode = $subnode::from_tokens(&mut ts)?;)?
                            $(let $ctoken = expect_token!(ts, $ctoken($pat): $ty);)?
                        )*
                        Some(($($($subnode,)?$($ctoken,)?)*))
                    })();
                    if out.is_some() {
                        *iter = ts;
                    }
                    out
                } {
                    return Some($node::$case($($($subnode,)?$($ctoken,)?)*));
                })*

                None
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
    Program(Function : ~<Function>);
    Function(IntFn :
        ~KeywordInt ~<Identifier> ~OpenParen ~KeywordVoid ~CloseParen ~OpenBrace
            ~<Statement>
        ~CloseBrace);
    Statement(ReturnStatement : ~KeywordReturn ~<Exp> ~Semicolon);
    Exp(Int : ~<Int>);
    Identifier(Variable : ~{Ident(_): String});
    Int(Constant : ~{Constant(_): isize});
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

    #[test]
    fn parse_int() {
        assert_eq!(
            Int::from_tokens(&mut [Constant(2)].into_iter()),
            Some(Int::Constant(2))
        );
    }

    #[test]
    fn parse_ident() {
        assert_eq!(
            Identifier::from_tokens(&mut [Ident("x".into())].into_iter()),
            Some(Identifier::Variable("x".into())),
        );
    }

    #[test]
    fn parse_exp() {
        assert_eq!(
            Exp::from_tokens(&mut [Constant(2)].into_iter()),
            Some(Exp::Int(Int::Constant(2)))
        );
    }

    #[test]
    fn parse_statement() {
        assert_eq!(
            Statement::from_tokens(&mut [KeywordReturn, Constant(2), Semicolon].into_iter()),
            Some(Statement::ReturnStatement(Exp::Int(Int::Constant(2))))
        );
    }
}
