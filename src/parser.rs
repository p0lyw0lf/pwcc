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
        *KeywordInt *{name: Ident(_ = String)} *OpenParen *KeywordVoid *CloseParen *OpenBrace
            *<statement: Statement>
        *CloseBrace
    );
    Statement(*KeywordReturn *<exp: Exp> *Semicolon);
    UnaryOp(+Minus +Tilde);
    BinaryOp(+Plus +Minus +Star +ForwardSlash +Percent);
}

/// Exp is special, since its AST doesn't exactly correspond with the grammar, so we define it
/// separately
#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Exp {
    Constant {
        constant: isize,
    },
    Unary {
        op: UnaryOp,
        exp: Box<Exp>,
    },
    Binary {
        lhs: Box<Exp>,
        op: BinaryOp,
        rhs: Box<Exp>,
    },
}

impl FromTokens for Exp {
    fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
        todo!()
    }
}

impl ToTokens for Exp {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        use Exp::*;
        use Token::*;
        let out: Box<dyn Iterator<Item = Token>> = match self {
            Exp::Constant { constant } => Box::new(core::iter::once(Token::Constant(constant))),
            Unary { op, exp } => Box::new(
                op.to_tokens()
                    .chain(core::iter::once(OpenParen))
                    .chain(exp.to_tokens())
                    .chain(core::iter::once(CloseParen)),
            ),
            Binary { lhs, op, rhs } => Box::new(
                core::iter::once(OpenParen)
                    .chain(lhs.to_tokens())
                    .chain(core::iter::once(CloseParen))
                    .chain(op.to_tokens())
                    .chain(core::iter::once(OpenParen))
                    .chain(rhs.to_tokens())
                    .chain(core::iter::once(CloseParen)),
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
    fn exp() {
        assert_convertible(&[Constant(2)], Exp::Constant { constant: 2 });
    }

    #[test]
    fn statement() {
        assert_convertible(
            &[KeywordReturn, Constant(2), Semicolon],
            Statement {
                exp: Exp::Constant { constant: 2 },
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
                name: "main".into(),
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
                        exp: Box::new(Exp::Constant { constant: 3 }),
                    }),
                }),
            },
        )
    }
}
