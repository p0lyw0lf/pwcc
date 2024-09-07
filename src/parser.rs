use core::convert::From;
use core::fmt::Debug;
use core::iter::Iterator;

use crate::lexer::Token;

mod macros;
use macros::*;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ParseError {
    UnexpectedToken { expected: Token, actual: Token },
    MissingToken { expected: Token },
    ExtraToken { actual: Token },
    NoMatches,
}

pub trait FromTokens: Sized {
    fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError>;
}

// IntoIterator is too hard b/c can't name the IntoIter type
pub trait ToTokens {
    fn to_tokens(self) -> impl Iterator<Item = Token>;
}

nodes! {
    Program(*<function: Function>);
    Function(
        *KeywordInt *<identifier: Identifier> *OpenParen *KeywordVoid *CloseParen *OpenBrace
            *<statement: Statement>
        *CloseBrace
    );
    Statement(*KeywordReturn *<exp: Exp> *Semicolon);
    Exp(
        +IntExp(*<int: Int>)
        +Unary(*<op: UnaryOp> *[exp: Box<Self>])
        +( *OpenParen *[Self] *CloseParen )
    );
    UnaryOp(+Minus +Tilde);
    Identifier(*{ident: Ident(_ = String)});
    Int(*{constant: Constant(_ = isize)});
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
        assert_convertible(
            &[Constant(2)],
            Exp::IntExp {
                int: Int { constant: 2 },
            },
        );
    }

    #[test]
    fn statement() {
        assert_convertible(
            &[KeywordReturn, Constant(2), Semicolon],
            Statement {
                exp: Exp::IntExp {
                    int: Int { constant: 2 },
                },
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
                    exp: Exp::IntExp {
                        int: Int { constant: 2 },
                    },
                },
            },
        );
    }

    #[test]
    fn unary() {
        assert_convertible(
            &[Minus, Minus, Tilde, Constant(3)],
            Exp::Unary {
                op: UnaryOp::Minus,
                exp: Box::new(Exp::Unary {
                    op: UnaryOp::Minus,
                    exp: Box::new(Exp::Unary {
                        op: UnaryOp::Tilde,
                        exp: Box::new(Exp::IntExp {
                            int: Int { constant: 3 },
                        }),
                    }),
                }),
            },
        )
    }
}
