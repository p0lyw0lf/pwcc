use core::convert::From;
use core::fmt::Debug;
use core::iter::Iterator;

use crate::lexer::Token;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ParseError {
    UnexpectedToken { expected: Token, actual: Token },
    MissingToken { expected: Token },
    ExtraToken { actual: Token },
    NoMatches,
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
    // Multiplication: concatenate all nodes
    ($node:ident ( $(
        *
        $($itoken:ident)?
        $(<$sname:ident : $subnode:ident>)?
        $({$cname:ident : $ctoken:ident ($pat:pat = $ty:ty) })?
    )* ) ) => {
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub struct $node {$(
            $(pub $sname: $subnode,)?
            $(pub $cname: $ty,)?
        )*}

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
                $(
                    $(expect_token!(ts, $itoken);)?
                    $(let $sname = $subnode::from_tokens(ts)?;)?
                    $(let $cname = expect_token!(ts, $ctoken($pat): $ty);)?
                )*
                Ok($node {$(
                    $($sname,)?
                    $($cname,)?
                )*})
            }
        }

        impl ToTokens for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                let $node {$(
                    $($sname,)?
                    $($cname,)?
                )*} = self;

                ::core::iter::empty()
                $(.chain(
                    $(::core::iter::once(Token::$itoken))?
                    $($sname.to_tokens())?
                    $(::core::iter::once(Token::$ctoken($cname.into())))?
                ))*
            }
        }
    };

    // Addition: choose between notes
    ($node:ident ( $(
        +
        $($itoken:ident)?
        $(<$subnode:ident>)?
        $({$ctoken:ident ($pat:pat = $ty:ty) })?
    )* ) ) => {
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub enum $node {$(
            $($itoken,)?
            $($subnode($subnode),)?
            $($ctoken($ty),)?
        )*}

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
                use $node::*;
                $(
                let mut iter = ts.clone();
                if let Ok(out) = (|| -> Result<Self, ParseError> {
                    $(let out = {
                        expect_token!(iter, $itoken);
                        $itoken
                    };)?
                    $(let out = $subnode($subnode.from_tokens(&mut iter)?);)?
                    $(let out = $ctoken(expect_token!(iter, $ctoken($pat): $ty));)?
                    Ok(out)
                })() {
                    *ts = iter;
                    return Ok(out);
                }
                )*

                Err(ParseError::NoMatches)
            }
        }

        impl ToTokens for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                use $node::*;
                match self {$(
                    $($itoken => Box::new(::core::iter::once(Token::$itoken)) as Box<dyn Iterator<Item = Token>>,)?
                    $($subnode(s) => Box::new(s.to_tokens()) as Box<dyn Iterator<Item = Token>>,)?
                    $($ctoken(c) => Box::new(::core::iter::once(Token::$ctoken(c.into()))) as Box<dyn Iterator<Item = Token>>,)?
                )*}
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
    Program(*<function: Function>);
    Function(
        *KeywordInt *<identifier: Identifier> *OpenParen *KeywordVoid *CloseParen *OpenBrace
            *<statement: Statement>
        *CloseBrace
    );
    Statement(*KeywordReturn *<exp: Exp> *Semicolon);
    // Exp needs to be implemented separately because of parenthesis
    UnaryOp(+Minus +Tilde);
    Identifier(*{ident: Ident(_ = String)});
    Int(*{constant: Constant(_ = isize)});
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Exp {
    Int(Int),
    Unary { op: UnaryOp, exp: Box<Exp> },
}

impl FromTokens for Exp {
    fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
        let mut iter = ts.clone();
        if let Ok(out) = Int::from_tokens(&mut iter) {
            *ts = iter;
            return Ok(Exp::Int(out));
        }

        let mut iter = ts.clone();
        if let Ok(out) = (|| -> Result<Self, ParseError> {
            let op = UnaryOp::from_tokens(&mut iter)?;
            let exp = Exp::from_tokens(&mut iter)?;
            Ok(Exp::Unary {
                op,
                exp: Box::new(exp),
            })
        })() {
            *ts = iter;
            return Ok(out);
        }

        if let Ok(out) = (|| -> Result<Self, ParseError> {
            expect_token!(iter, OpenParen);
            let exp = Exp::from_tokens(&mut iter)?;
            expect_token!(iter, CloseParen);
            Ok(exp)
        })() {
            *ts = iter;
            return Ok(out);
        }

        Err(ParseError::NoMatches)
    }
}

impl ToTokens for Exp {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        use Exp::*;
        let out: Box<dyn Iterator<Item = Token>> = match self {
            Int(i) => Box::new(i.to_tokens()),
            Unary { op, exp } => Box::new(
                op.to_tokens()
                    .chain(core::iter::once(Token::OpenParen))
                    .chain(exp.to_tokens())
                    .chain(core::iter::once(Token::CloseParen)),
            ),
        };
        out
    }
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
        assert_convertible(&[Constant(2)], Int { constant: 2 });
    }

    #[test]
    fn ident() {
        assert_convertible(&[Ident("x".into())], Identifier { ident: "x".into() });
    }

    #[test]
    fn exp() {
        assert_convertible(&[Constant(2)], Exp::Int(Int { constant: 2 }));
    }

    #[test]
    fn statement() {
        assert_convertible(
            &[KeywordReturn, Constant(2), Semicolon],
            Statement {
                exp: Exp::Int(Int { constant: 2 }),
            },
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
            Function {
                identifier: Identifier {
                    ident: "main".into(),
                },
                statement: Statement {
                    exp: Exp::Int(Int { constant: 2 }),
                },
            },
        );
    }
}
