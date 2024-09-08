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

impl BinaryOp {
    fn precedence(&self) -> usize {
        use BinaryOp::*;
        match self {
            Plus | Minus => 45,
            Star | ForwardSlash | Percent => 50,
        }
    }
}

impl FromTokens for Exp {
    fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
        fn parse_factor(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Exp, ParseError> {
            let mut iter = ts.clone();
            if let Ok(out) = (|| -> Result<Exp, ParseError> {
                let constant = expect_token!(iter, Constant(_): isize);
                Ok(Exp::Constant { constant })
            })() {
                *ts = iter;
                return Ok(out);
            }

            let mut iter = ts.clone();
            if let Ok(out) = (|| -> Result<Exp, ParseError> {
                let op = UnaryOp::from_tokens(&mut iter)?;
                let exp = parse_factor(&mut iter)?;
                Ok(Exp::Unary {
                    op,
                    exp: Box::new(exp),
                })
            })() {
                *ts = iter;
                return Ok(out);
            }

            let mut iter = ts.clone();
            if let Ok(out) = (|| -> Result<Exp, ParseError> {
                expect_token!(iter, OpenParen);
                let exp = parse_exp(&mut iter, 0)?;
                expect_token!(iter, CloseParen);
                Ok(exp)
            })() {
                *ts = iter;
                return Ok(out);
            }

            Err(ParseError::NoMatches)
        }

        fn parse_exp(
            ts: &mut (impl Iterator<Item = Token> + Clone),
            min_prec: usize,
        ) -> Result<Exp, ParseError> {
            let mut iter = ts.clone();
            let mut left = parse_factor(&mut iter)?;

            let mut peek_iter = iter.clone();
            let mut next_token = BinaryOp::from_tokens(&mut peek_iter);
            while let Ok(op) = next_token {
                if op.precedence() < min_prec {
                    break;
                }
                iter = peek_iter;
                let right = parse_exp(&mut iter, op.precedence() + 1)?;
                left = Exp::Binary {
                    lhs: Box::new(left),
                    op,
                    rhs: Box::new(right),
                };

                peek_iter = iter.clone();
                next_token = BinaryOp::from_tokens(&mut peek_iter);
            }

            *ts = iter;
            Ok(left)
        }

        parse_exp(ts, 0)
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

    fn assert_convertible(s: &str, tree: impl ToTokens + FromTokens + Debug + PartialEq) {
        let tokens = crate::lexer::lex(s).expect("lex failed");
        assert_forwards(&tokens, &tree);
        assert_backwards(tree, &tokens);
    }

    #[test]
    fn constant() {
        assert_convertible("2", Exp::Constant { constant: 2 });
    }

    #[test]
    fn statement() {
        assert_convertible(
            "return 2;",
            Statement {
                exp: Exp::Constant { constant: 2 },
            },
        );
    }

    #[test]
    fn function() {
        assert_convertible(
            "int main(void) { return 2; }",
            Function {
                name: "main".into(),
                statement: Statement {
                    exp: Exp::Constant { constant: 2 },
                },
            },
        );
    }

    #[test]
    fn unary() {
        assert_convertible(
            "-(-(~(3)))",
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

    #[test]
    fn binary() {
        assert_convertible(
            "((1)+(2))*(3)",
            Exp::Binary {
                lhs: Box::new(Exp::Binary {
                    lhs: Box::new(Exp::Constant { constant: 1 }),
                    op: BinaryOp::Plus,
                    rhs: Box::new(Exp::Constant { constant: 2 }),
                }),
                op: BinaryOp::Star,
                rhs: Box::new(Exp::Constant { constant: 3 }),
            },
        );
    }

    #[test]
    fn binary_precedence() {
        let tokens = crate::lexer::lex("1*2-3*(4+5)").expect("lex failed");
        assert_forwards(
            &tokens,
            &Exp::Binary {
                lhs: Box::new(Exp::Binary {
                    lhs: Box::new(Exp::Constant { constant: 1 }),
                    op: BinaryOp::Star,
                    rhs: Box::new(Exp::Constant { constant: 2 }),
                }),
                op: BinaryOp::Minus,
                rhs: Box::new(Exp::Binary {
                    lhs: Box::new(Exp::Constant { constant: 3 }),
                    op: BinaryOp::Star,
                    rhs: Box::new(Exp::Binary {
                        lhs: Box::new(Exp::Constant { constant: 4 }),
                        op: BinaryOp::Plus,
                        rhs: Box::new(Exp::Constant { constant: 5 }),
                    }),
                }),
            },
        )
    }
}
