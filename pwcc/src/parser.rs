use core::convert::From;
use core::fmt::Debug;
use core::iter::Iterator;

use crate::lexer::SpanToken;
use crate::lexer::Token;

mod errors;
mod macros;
mod traits;

pub use errors::ParseError;
use macros::*;
pub use traits::*;

#[cfg(test)]
mod test;
#[cfg(test)]
mod test_errors;

nodes! {
    Program(*<function: Function>);
    Function(
        *KeywordInt *{name: Ident(_ = String)} *OpenParen *KeywordVoid *CloseParen
            *<body: Block>
    );
    Block(*OpenBrace *<items: Vec<BlockItem>> *CloseBrace);
    BlockItem(+<Statement> +<Declaration>);
    Statement(+<ExpressionStmt> +<IfStmt> +<LabelStmt> +<GotoStmt> +<ReturnStmt> +<Block> +<NullStmt>);
    ExpressionStmt(*<exp: Exp> *Semicolon);
    IfStmt(*KeywordIf *OpenParen *<exp: Exp> *CloseParen *<body: Box<Statement>> *<else_stmt: Option<ElseStmt>>);
    ElseStmt(*KeywordElse *<body: Box<Statement>>);
    LabelStmt(*{label: Ident(_ = String)} *Colon);
    GotoStmt(*KeywordGoto *{label: Ident(_ = String)} *Semicolon);
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
    BinaryTok(
        +<BinaryOp>
        +<AssignmentOp>
        // Not a "true" binary operator, but we do parse it like one. This will result in trying to
// parse a ternary expression.
        +Question
    );

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
        Ternary {
            condition: Box<Exp>,
            true_case: Box<Exp>,
            false_case: Box<Exp>,
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
    Ternary,
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
    // Prefix,  // Taken care of by parse_unary
    // Postfix, // Taken care of by parse_postfix
    #[allow(dead_code)]
    Literal, // Taken care of by parse_primary
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
            BinaryTok::Question => Precedence::Ternary,
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
    fn from_tokens(ts: &mut (impl Iterator<Item = SpanToken> + Clone)) -> Result<Self, ParseError> {
        fn parse_primary(
            ts: &mut (impl Iterator<Item = SpanToken> + Clone),
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
            ts: &mut (impl Iterator<Item = SpanToken> + Clone),
        ) -> Result<Exp, ParseError> {
            let mut iter = ts.clone();
            let mut exp = parse_primary(&mut iter)?;

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

        fn parse_unary(
            ts: &mut (impl Iterator<Item = SpanToken> + Clone),
        ) -> Result<Exp, ParseError> {
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
            ts: &mut (impl Iterator<Item = SpanToken> + Clone),
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
                    BinaryTok::Question => {
                        let middle = parse_exp(&mut iter, Precedence::lowest())?;
                        expect_token!(iter, Colon);
                        let right = parse_exp(&mut iter, prec)?;
                        left = Exp::Ternary {
                            condition: left.boxed(),
                            true_case: middle.boxed(),
                            false_case: right.boxed(),
                        }
                    }
                }

                peek_iter = iter.clone();
                next_token = BinaryTok::from_tokens(&mut peek_iter);
            }

            *ts = iter;
            Ok(left)
        }

        parse_exp(ts, Precedence::lowest()).map_err(|e| ParseError::Context {
            node_name: "Exp".to_string(),
            err: Box::new(e),
        })
    }
}

impl ToTokens for Exp {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        use Exp::*;
        use Token::*;
        let out: Box<dyn Iterator<Item = Token>> = match self {
            Exp::Constant { constant } => Box::new(core::iter::once(Token::Constant(constant))),
            Var { ident } => Box::new(core::iter::once(Ident(ident))),
            Unary { op, exp } => match op {
                UnaryOp::PrefixOp(op) => Box::new(
                    op.to_tokens()
                        .chain(core::iter::once(OpenParen))
                        .chain(exp.to_tokens())
                        .chain(core::iter::once(CloseParen)),
                ),
                UnaryOp::PostfixOp(op) => Box::new(
                    core::iter::once(OpenParen)
                        .chain(exp.to_tokens())
                        .chain(core::iter::once(CloseParen))
                        .chain(op.to_tokens()),
                ),
            },
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
            Ternary {
                condition,
                true_case,
                false_case,
            } => Box::new(
                core::iter::once(OpenParen)
                    .chain(condition.to_tokens())
                    .chain([CloseParen, Question, OpenParen].into_iter())
                    .chain(true_case.to_tokens())
                    .chain([CloseParen, Colon, OpenParen])
                    .chain(false_case.to_tokens())
                    .chain(core::iter::once(CloseParen)),
            ),
        };
        out
    }
}

pub fn parse<TS>(tokens: TS) -> Result<Program, ParseError>
where
    TS: IntoIterator<Item = SpanToken>,
    TS::IntoIter: Clone,
{
    let mut iter = tokens.into_iter();
    Program::from_tokens(&mut iter).and_then(|p| match iter.next() {
        Some(token) => Err(ParseError::ExtraToken { actual: token }),
        None => Ok(p),
    })
}
