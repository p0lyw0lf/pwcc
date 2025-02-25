use core::convert::From;
use core::fmt::Debug;
use core::iter::Iterator;

use crate::lexer::Token;

mod macros;
use macros::*;

#[cfg(test)]
mod test;

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
            *<body: Body>
        *CloseBrace
    );
    Body[BlockItem];
    BlockItem(+<Statement> +<Declaration>);
    Statement(+<ExpressionStmt> +<ReturnStmt> +<NullStmt>);
    ExpressionStmt(*<exp: Exp> *Semicolon);
    ReturnStmt(*KeywordReturn *<exp: Exp> *Semicolon);
    NullStmt(*Semicolon);
    Declaration(*KeywordInt *{name: Ident(_ = String)} *<init: Initializer>);
    Initializer(+<NoInit> +<ExpressionInit>);
    NoInit(*Semicolon);
    ExpressionInit(*Equal *<exp: Exp> *Semicolon);
    // Exp defined separately
    UnaryOp(+Minus +Tilde +Exclamation);
    BinaryOp(
        // Arithmetic operators
        +Plus +Minus +Star +ForwardSlash +Percent
        // Bitwise operators
        +LeftShift +RightShift +Ampersand +Caret +Pipe
        // Logical operators
        +DoubleAmpersand +DoublePipe +DoubleEqual +NotEqual +LessThan +LessThanEqual +GreaterThan +GreaterThanEqual
    );
    AssignmentOp(
        // Standard assignment
        +Equal
        // Arithmetic assignment
        +PlusEqual +MinusEqual +StarEqual +ForwardSlashEqual +PercentEqual
        // Bitwise assignment
        +LeftShiftEqual +RightShiftEqual +AmpersandEqual +CaretEqual +PipeEqual
    );
    // Not used in grammar, only for parsing Exp
    BinaryTok(+<BinaryOp> +<AssignmentOp>);

    // Exp is special, since its AST doesn't exactly correspond with the grammar, so we define it
    // separately
    Exp enum {
        Constant {
            constant: isize,
        },
        Var {
            ident: String,
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
        Assignment {
            lhs: Box<Exp>, // Semantic analysis ensures this is a proper LValue
            op: AssignmentOp,
            rhs: Box<Exp>,
        },
    };
}

impl AssignmentOp {
    pub fn to_binary_op(self) -> Option<BinaryOp> {
        use AssignmentOp::*;
        if let Equal = self {
            return None;
        }
        Some(match self {
            Equal => unreachable!(),
            PlusEqual => BinaryOp::Plus,
            MinusEqual => BinaryOp::Minus,
            StarEqual => BinaryOp::Star,
            ForwardSlashEqual => BinaryOp::ForwardSlash,
            PercentEqual => BinaryOp::Percent,
            LeftShiftEqual => BinaryOp::LeftShift,
            RightShiftEqual => BinaryOp::RightShift,
            AmpersandEqual => BinaryOp::Ampersand,
            CaretEqual => BinaryOp::Caret,
            PipeEqual => BinaryOp::Pipe,
        })
    }
}

impl BinaryTok {
    /// Based on https://en.cppreference.com/w/c/language/operator_precedence, going in reverse
    /// order.
    fn precedence(&self) -> usize {
        use BinaryOp::*;
        16 - match self {
            BinaryTok::BinaryOp(op) => match op {
                Star | ForwardSlash | Percent => 3,
                Plus | Minus => 4,
                LeftShift | RightShift => 5,
                LessThan | LessThanEqual | GreaterThan | GreaterThanEqual => 6,
                DoubleEqual | NotEqual => 7,
                Ampersand => 8,
                Caret => 9,
                Pipe => 10,
                DoubleAmpersand => 11,
                DoublePipe => 12,
            },
            BinaryTok::AssignmentOp(_) => 14,
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
                let ident = expect_token!(iter, Ident(_): String);
                Ok(Exp::Var { ident })
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
            let mut next_token = BinaryTok::from_tokens(&mut peek_iter);
            while let Ok(op) = next_token {
                let prec = op.precedence();
                if prec < min_prec {
                    break;
                }
                iter = peek_iter;

                match op {
                    BinaryTok::BinaryOp(op) => {
                        // Left-associative
                        let right = parse_exp(&mut iter, prec + 1)?;
                        left = Exp::Binary {
                            lhs: Box::new(left),
                            op,
                            rhs: Box::new(right),
                        };
                    }
                    BinaryTok::AssignmentOp(op) => {
                        // Right-associative
                        let right = parse_exp(&mut iter, prec)?;
                        left = Exp::Assignment {
                            lhs: Box::new(left),
                            op,
                            rhs: Box::new(right),
                        };
                    }
                }

                peek_iter = iter.clone();
                next_token = BinaryTok::from_tokens(&mut peek_iter);
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
            Var { ident } => Box::new(core::iter::once(Ident(ident))),
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
            Assignment { lhs, op, rhs } => {
                Box::new(lhs.to_tokens().chain(op.to_tokens()).chain(rhs.to_tokens()))
            }
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
