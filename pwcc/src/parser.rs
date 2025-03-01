use core::convert::From;
use core::fmt::Debug;
use core::iter::Iterator;

use miette::Diagnostic;
use thiserror::Error;

use crate::lexer::Token;

mod macros;
use macros::*;

#[cfg(test)]
mod test;

#[derive(Error, Diagnostic, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ParseError {
    #[error("Unexpected token: expected {expected:-}, got {actual}")]
    UnexpectedToken { expected: Token, actual: Token },
    #[error("Missing token: expected {expected}")]
    MissingToken { expected: Token },
    #[error("Expected end of tokens, got {actual}")]
    ExtraToken { actual: Token },
    #[error("No matches found for tokens")]
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
    PrefixOp(+Minus +Tilde +Exclamation +Increment +Decrement);
    PostfixOp(+Increment +Decrement);
    UnaryOp(+<PrefixOp> +<PostfixOp>);
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

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum Precedence {
    // Sequence, // Not implemented
    Assignment,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equal,
    Compare,
    Shift,
    Addition,
    Multiplication,
    Prefix,  // Taken care of by parse_unary
    Postfix, // Taken care of by parse_postfix
    Literal,
}

impl Precedence {
    fn lowest() -> Self {
        Precedence::Assignment
    }

    fn next(self) -> Self {
        if let Precedence::Literal = self {
            return self;
        }

        // SAFETY: if we are not at the last precedence value, there will always be another.
        unsafe { core::mem::transmute((self as u8) + 1) }
    }
}

impl BinaryTok {
    /// Based on https://en.cppreference.com/w/c/language/operator_precedence, going in reverse
    /// order.
    fn precedence(&self) -> Precedence {
        use BinaryOp::*;
        match self {
            BinaryTok::BinaryOp(op) => match op {
                Star | ForwardSlash | Percent => Precedence::Multiplication,
                Plus | Minus => Precedence::Addition,
                LeftShift | RightShift => Precedence::Shift,
                LessThan | LessThanEqual | GreaterThan | GreaterThanEqual => Precedence::Compare,
                DoubleEqual | NotEqual => Precedence::Equal,
                Ampersand => Precedence::BitwiseAnd,
                Caret => Precedence::BitwiseXor,
                Pipe => Precedence::BitwiseOr,
                DoubleAmpersand => Precedence::LogicalAnd,
                DoublePipe => Precedence::LogicalOr,
            },
            BinaryTok::AssignmentOp(_) => Precedence::Assignment,
        }
    }
}

impl Exp {
    // Helper method, since we'll be doing this a lot
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

impl FromTokens for Exp {
    fn from_tokens(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Self, ParseError> {
        fn parse_primary(
            ts: &mut (impl Iterator<Item = Token> + Clone),
        ) -> Result<Exp, ParseError> {
            try_parse!(
                ts,
                Err(ParseError::NoMatches),
                |iter| {
                    let constant = expect_token!(iter, Constant(_): isize);
                    Ok(Exp::Constant { constant })
                },
                |iter| {
                    let ident = expect_token!(iter, Ident(_): String);
                    Ok(Exp::Var { ident })
                },
                |iter| {
                    expect_token!(iter, OpenParen);
                    let exp = parse_exp(&mut iter, Precedence::lowest())?;
                    expect_token!(iter, CloseParen);
                    Ok(exp)
                },
            )
        }

        fn parse_postfix(
            ts: &mut (impl Iterator<Item = Token> + Clone),
        ) -> Result<Exp, ParseError> {
            let mut iter = ts.clone();
            let mut exp = parse_primary(ts)?;

            let mut peek_iter = iter.clone();
            let mut next_token = PostfixOp::from_tokens(&mut peek_iter);
            while let Ok(op) = next_token {
                iter = peek_iter;

                // Left-associative
                exp = Exp::Unary {
                    op: UnaryOp::PostfixOp(op),
                    exp: exp.boxed(),
                };

                peek_iter = iter.clone();
                next_token = PostfixOp::from_tokens(&mut peek_iter);
            }

            *ts = iter;
            Ok(exp)
        }

        fn parse_unary(ts: &mut (impl Iterator<Item = Token> + Clone)) -> Result<Exp, ParseError> {
            try_parse!(
                ts,
                Err(ParseError::NoMatches),
                |iter| {
                    let prefix = PrefixOp::from_tokens(&mut iter)?;
                    let exp = parse_unary(&mut iter)?;
                    Ok(Exp::Unary {
                        op: UnaryOp::PrefixOp(prefix),
                        exp: exp.boxed(),
                    })
                },
                |iter| { parse_postfix(&mut iter) },
            )
        }

        fn parse_exp(
            ts: &mut (impl Iterator<Item = Token> + Clone),
            min_prec: Precedence,
        ) -> Result<Exp, ParseError> {
            let mut iter = ts.clone();
            let mut left = parse_unary(&mut iter)?;

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
                        let right = parse_exp(&mut iter, prec.next())?;
                        left = Exp::Binary {
                            lhs: left.boxed(),
                            op,
                            rhs: right.boxed(),
                        };
                    }
                    BinaryTok::AssignmentOp(op) => {
                        // Right-associative
                        let right = parse_exp(&mut iter, prec)?;
                        left = Exp::Assignment {
                            lhs: left.boxed(),
                            op,
                            rhs: right.boxed(),
                        };
                    }
                }

                peek_iter = iter.clone();
                next_token = BinaryTok::from_tokens(&mut peek_iter);
            }

            *ts = iter;
            Ok(left)
        }

        parse_exp(ts, Precedence::lowest())
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
