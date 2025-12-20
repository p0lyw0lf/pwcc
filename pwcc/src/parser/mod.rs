use core::convert::From;
use core::iter::Iterator;
use core::iter::once as one;

use functional::Semigroup;

use crate::lexer::Token;
use crate::parser::helpers::CommaDelimited;
use crate::span::Span;
use crate::span::Spanned;

mod errors;
mod helpers;

use errors::ParseError;

#[cfg(test)]
mod test;
#[cfg(test)]
mod test_errors;

// TODO: I probably should make it so the enums are not so huge sometimes...
nodes! {
    {
        use crate::lexer::Token;
        use crate::parser::errors::ParseError;
        use crate::parser::helpers::CommaDelimited;
        use crate::parser::macros::expect_token;
        use crate::parser::macros::parse_choices;
        use crate::parser::traits::FromTokens;
        use crate::parser::traits::ToTokens;
        use crate::span::Span;
        use crate::span::Spanned;
        use std::collections::BTreeMap;
    }
    Program(*<functions: Vec<FunctionDecl>>);
    Block(*OpenBrace *<items: Vec<BlockItem>> *CloseBrace) [include];
    BlockItem(+<Statement> +<Declaration>) [include];
    Statement(
        +<ReturnStmt>
        +<ExpressionStmt>
        +<IfStmt>
        +<BreakStmt>
        +<ContinueStmt>
        +<WhileStmt>
        +<DoWhileStmt>
        +<ForStmt>
        +<SwitchStmt>
        +<Block>
        +<LabelStmt>
        +<GotoStmt>
        +<NullStmt>
    ) [include];
    ReturnStmt(*KeywordReturn *<exp: Exp> *Semicolon);
    ExpressionStmt(*<exp: Exp> *Semicolon);

    IfStmt(*KeywordIf *OpenParen *<guard: Exp> *CloseParen *<body: Box<Statement>> *<else_stmt: Option<ElseStmt>>);
    ElseStmt(*KeywordElse *<body: Box<Statement>>);
    SwitchStmt(*KeywordSwitch *<ctx: Option<SwitchContext>> *<label: Option<LoopLabel>> *OpenParen *<exp: Exp> *CloseParen *<body: Box<Statement>>);

    BreakStmt(*KeywordBreak *<label: Option<LoopLabel>> *Semicolon);
    ContinueStmt(*KeywordContinue *<label: Option<LoopLabel>> *Semicolon);
    WhileStmt(*KeywordWhile *<label: Option<LoopLabel>> *OpenParen *<guard: Exp> *CloseParen *<body: Box<Statement>>);
    DoWhileStmt(*KeywordDo *<body: Box<Statement>> *KeywordWhile *<label: Option<LoopLabel>> *OpenParen *<guard: Exp> *CloseParen *Semicolon);
    ForStmt(*KeywordFor *<label: Option<LoopLabel>> *OpenParen *<init: ForInit> *<exp1: Option<Exp>> *Semicolon *<exp2: Option<Exp>> *CloseParen *<body: Box<Statement>>);
    ForInit(+<VarDecl> +<ForInitExp>);
    ForInitExp(*<exp: Option<Exp>> *Semicolon);

    LabelStmt(*<label: Label> *Colon *<stmt: Box<Statement>>);
    Label(
        +<RawLabel>
        +<CaseLabel>
    );
    RawLabel(*{label: Ident(_ = String)}) [include];

    GotoStmt(*KeywordGoto *{label: Ident(_ = String)} *Semicolon) [include];

    NullStmt(*Semicolon);

    Declaration(+<FunctionDecl> +<VarDecl>);
    FunctionDecl(
        *KeywordInt *{name: Ident(_ = String)} *OpenParen *<args: FunctionDeclArgs> *CloseParen
            *<body: FunctionBody>
    ) [include];
    FunctionDeclArgs(+KeywordVoid +<DeclArgs>);
    DeclArgs(*<args: CommaDelimited<DeclArg>>);
    DeclArg(*KeywordInt *{name: Ident(_ = String)});
    FunctionBody(+Semicolon +<Block>);
    VarDecl(*KeywordInt *{name: Ident(_ = String)} *<init: Initializer>);
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
            span: Span,
        },
        Var {
            // I don't really want to do this, but to get unused variables to work right, I have no
            // other choice unfortunately...
            ident: String,
            span: Span,
        },
        Unary {
            op: UnaryOp,
            exp: Box<Exp>,
            span: Span,
        },
        Binary {
            lhs: Box<Exp>,
            op: BinaryOp,
            rhs: Box<Exp>,
            span: Span,
        },
        Ternary {
            condition: Box<Exp>,
            true_case: Box<Exp>,
            false_case: Box<Exp>,
            span: Span,
        },
        Assignment {
            lhs: Box<Exp>, // Semantic analysis ensures this is a proper LValue
            op: AssignmentOp,
            rhs: Box<Exp>,
            span: Span,
        },
        FunctionCall {
            ident: (String, Span),
            args: CommaDelimited<Exp>,
            span: Span,
        },
    } [include];

    // Similarly for LoopLabel; we don't want to be able to parse them, but we do want to be able
    // to represent them.
    LoopLabel struct (pub String);

    CaseLabel enum {
        Case(Exp),
        Default(Span),
        Labeled(String, Span),
    };
    // Maps the constant-evaluated case value (if applicable) to the case it should jump to if the
    // value matches.
    SwitchContext struct (pub BTreeMap<Option<isize>, (String, Span)>);
}

impl FunctionDeclArgs {
    pub fn num_args(&self) -> usize {
        match self {
            FunctionDeclArgs::KeywordVoid(_) => 0,
            FunctionDeclArgs::DeclArgs(DeclArgs { args, span: _ }) => args.0.len(),
        }
    }
}

impl IntoIterator for FunctionDeclArgs {
    type Item = DeclArg;
    type IntoIter = <Vec<Self::Item> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            FunctionDeclArgs::KeywordVoid(_) => vec![].into_iter(),
            FunctionDeclArgs::DeclArgs(DeclArgs { args, span: _ }) => args.0.into_iter(),
        }
    }
}

impl FunctionDeclArgs {
    pub fn iter(&self) -> std::slice::Iter<'_, DeclArg> {
        match self {
            FunctionDeclArgs::KeywordVoid(_) => [].iter(),
            FunctionDeclArgs::DeclArgs(DeclArgs { args, span: _ }) => args.0.iter(),
        }
    }
}

impl Spanned for LoopLabel {
    fn span(&self) -> Span {
        Span::empty()
    }
}

impl FromTokens for LoopLabel {
    fn from_tokens(
        _ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
    ) -> Result<Self, ParseError> {
        Err(ParseError::NoMatches)
    }
}

impl ToTokens for LoopLabel {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        one(Token::Ident(self.0))
    }
}

impl Spanned for CaseLabel {
    fn span(&self) -> Span {
        use CaseLabel::*;
        match self {
            Case(exp) => exp.span(),
            Default(span) => *span,
            Labeled(_, span) => *span,
        }
    }
}

impl FromTokens for CaseLabel {
    fn from_tokens(
        ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
    ) -> Result<Self, ParseError> {
        parse_choices!(
            ts,
            Err(ParseError::NoMatches),
            |iter| {
                let mut _span = Span::empty();
                expect_token!(iter, _span, KeywordCase);
                let exp = Exp::from_tokens(&mut iter)?;
                Ok(CaseLabel::Case(exp))
            },
            |iter| {
                let mut span = Span::empty();
                expect_token!(iter, span, KeywordDefault);
                Ok(CaseLabel::Default(span))
            },
        )
    }
}

impl ToTokens for CaseLabel {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        let out: Box<dyn Iterator<Item = Token>> = match self {
            CaseLabel::Case(exp) => Box::new(one(Token::KeywordCase).chain(exp.to_tokens())),
            CaseLabel::Default(_) => Box::new(one(Token::KeywordDefault)),
            CaseLabel::Labeled(label, _) => Box::new(one(Token::Ident(label))),
        };
        out
    }
}

impl Spanned for SwitchContext {
    fn span(&self) -> Span {
        Span::empty()
    }
}

impl FromTokens for SwitchContext {
    fn from_tokens(
        _ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
    ) -> Result<Self, ParseError> {
        Err(ParseError::NoMatches)
    }
}

impl ToTokens for SwitchContext {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        core::iter::empty()
    }
}

impl AssignmentOp {
    pub fn to_binary_op(self) -> Option<BinaryOp> {
        use AssignmentOp::*;
        if let Equal(_) = self {
            return None;
        }
        Some(match self {
            Equal(_) => unreachable!(),
            PlusEqual(span) => BinaryOp::Plus(span),
            MinusEqual(span) => BinaryOp::Minus(span),
            StarEqual(span) => BinaryOp::Star(span),
            ForwardSlashEqual(span) => BinaryOp::ForwardSlash(span),
            PercentEqual(span) => BinaryOp::Percent(span),
            LeftShiftEqual(span) => BinaryOp::LeftShift(span),
            RightShiftEqual(span) => BinaryOp::RightShift(span),
            AmpersandEqual(span) => BinaryOp::Ampersand(span),
            CaretEqual(span) => BinaryOp::Caret(span),
            PipeEqual(span) => BinaryOp::Pipe(span),
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
                Star(_) | ForwardSlash(_) | Percent(_) => Precedence::Multiplication,
                Plus(_) | Minus(_) => Precedence::Addition,
                LeftShift(_) | RightShift(_) => Precedence::Shift,
                LessThan(_) | LessThanEqual(_) | GreaterThan(_) | GreaterThanEqual(_) => {
                    Precedence::Compare
                }
                DoubleEqual(_) | NotEqual(_) => Precedence::Equal,
                Ampersand(_) => Precedence::BitwiseAnd,
                Caret(_) => Precedence::BitwiseXor,
                Pipe(_) => Precedence::BitwiseOr,
                DoubleAmpersand(_) => Precedence::LogicalAnd,
                DoublePipe(_) => Precedence::LogicalOr,
            },
            BinaryTok::Question(_) => Precedence::Ternary,
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

impl Spanned for Exp {
    fn span(&self) -> Span {
        use Exp::*;
        match self {
            Constant { span, .. } => *span,
            Var { span, .. } => *span,
            Unary { span, .. } => *span,
            Binary { span, .. } => *span,
            Assignment { span, .. } => *span,
            Ternary { span, .. } => *span,
            FunctionCall { span, .. } => *span,
        }
    }
}

impl FromTokens for Exp {
    fn from_tokens(
        ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
    ) -> Result<Self, ParseError> {
        fn parse_primary(
            ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
        ) -> Result<Exp, ParseError> {
            parse_choices!(
                ts,
                Err(ParseError::NoMatches),
                |iter| {
                    let mut span = Span::empty();
                    let (constant, _) = expect_token!(iter, span, Constant(_): isize);
                    Ok(Exp::Constant { constant, span })
                },
                |iter| {
                    let mut span = Span::empty();
                    let ident = expect_token!(iter, span, Ident(_): String);

                    expect_token!(iter, span, OpenParen);
                    let args = CommaDelimited::<Exp>::from_tokens(&mut iter)?;
                    expect_token!(iter, span, CloseParen);

                    Ok(Exp::FunctionCall { ident, args, span })
                },
                |iter| {
                    let mut span = Span::empty();
                    let (ident, _) = expect_token!(iter, span, Ident(_): String);
                    Ok(Exp::Var { ident, span })
                },
                |iter| {
                    let mut _span = Span::empty();
                    expect_token!(iter, _span, OpenParen);
                    let exp = parse_exp(&mut iter, Precedence::lowest())?;
                    expect_token!(iter, _span, CloseParen);
                    // Don't include parenthesis in expression span
                    Ok(exp)
                },
            )
        }

        fn parse_postfix(
            ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
        ) -> Result<Exp, ParseError> {
            let mut iter = ts.clone();
            let mut exp = parse_primary(&mut iter)?;

            let mut peek_iter = iter.clone();
            let mut next_token = PostfixOp::from_tokens(&mut peek_iter);
            while let Ok(op) = next_token {
                iter = peek_iter;

                // Left-associative
                let mut span = op.span();
                span.sconcat(exp.span());
                exp = Exp::Unary {
                    op: UnaryOp::PostfixOp(op),
                    exp: Box::new(exp),
                    span,
                };

                peek_iter = iter.clone();
                next_token = PostfixOp::from_tokens(&mut peek_iter);
            }

            *ts = iter;
            Ok(exp)
        }

        fn parse_unary(
            ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
        ) -> Result<Exp, ParseError> {
            parse_choices!(
                ts,
                Err(ParseError::NoMatches),
                |iter| {
                    let prefix = PrefixOp::from_tokens(&mut iter)?;
                    let exp = parse_unary(&mut iter)?;
                    let mut span = prefix.span();
                    span.sconcat(exp.span());
                    Ok(Exp::Unary {
                        op: UnaryOp::PrefixOp(prefix),
                        exp: Box::new(exp),
                        span,
                    })
                },
                |iter| { parse_postfix(&mut iter) },
            )
        }

        fn parse_exp(
            ts: &mut (impl Iterator<Item = (Token, Span)> + Clone),
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

                let mut span = op.span();
                match op {
                    BinaryTok::BinaryOp(op) => {
                        // Left-associative
                        let right = parse_exp(&mut iter, prec.next())?;
                        span.sconcat(left.span());
                        span.sconcat(right.span());
                        left = Exp::Binary {
                            lhs: Box::new(left),
                            op,
                            rhs: Box::new(right),
                            span,
                        };
                    }
                    BinaryTok::AssignmentOp(op) => {
                        // Right-associative
                        let right = parse_exp(&mut iter, prec)?;
                        span.sconcat(left.span());
                        span.sconcat(right.span());
                        left = Exp::Assignment {
                            lhs: Box::new(left),
                            op,
                            rhs: Box::new(right),
                            span,
                        };
                    }
                    BinaryTok::Question(_) => {
                        let middle = parse_exp(&mut iter, Precedence::lowest())?;
                        expect_token!(iter, span, Colon);
                        let right = parse_exp(&mut iter, prec)?;
                        span.sconcat(left.span());
                        span.sconcat(middle.span());
                        span.sconcat(right.span());
                        left = Exp::Ternary {
                            condition: left.boxed(),
                            true_case: middle.boxed(),
                            false_case: right.boxed(),
                            span,
                        };
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
            Exp::Constant {
                constant,
                span: _span,
            } => Box::new(one(Token::Constant(constant))),
            Var { ident, span: _span } => Box::new(one(Ident(ident))),
            Unary {
                op,
                exp,
                span: _span,
            } => match op {
                UnaryOp::PrefixOp(op) => Box::new(
                    op.to_tokens()
                        .chain(one(OpenParen))
                        .chain(exp.to_tokens())
                        .chain(one(CloseParen)),
                ),
                UnaryOp::PostfixOp(op) => Box::new(
                    one(OpenParen)
                        .chain(exp.to_tokens())
                        .chain(one(CloseParen))
                        .chain(op.to_tokens()),
                ),
            },
            Binary {
                lhs,
                op,
                rhs,
                span: _span,
            } => Box::new(
                one(OpenParen)
                    .chain(lhs.to_tokens())
                    .chain(one(CloseParen))
                    .chain(op.to_tokens())
                    .chain(one(OpenParen))
                    .chain(rhs.to_tokens())
                    .chain(one(CloseParen)),
            ),
            Assignment {
                lhs,
                op,
                rhs,
                span: _span,
            } => Box::new(lhs.to_tokens().chain(op.to_tokens()).chain(rhs.to_tokens())),
            Ternary {
                condition,
                true_case,
                false_case,
                span: _span,
            } => Box::new(
                one(OpenParen)
                    .chain(condition.to_tokens())
                    .chain([CloseParen, Question, OpenParen])
                    .chain(true_case.to_tokens())
                    .chain([CloseParen, Colon, OpenParen])
                    .chain(false_case.to_tokens())
                    .chain(one(CloseParen)),
            ),
            FunctionCall {
                ident,
                args,
                span: _span,
            } => Box::new(
                one(Ident(ident.0))
                    .chain(one(OpenParen))
                    .chain(args.to_tokens())
                    .chain(one(CloseParen)),
            ),
        };
        out
    }
}

pub fn parse<TS>(tokens: TS) -> Result<Program, ParseError>
where
    TS: IntoIterator<Item = (Token, Span)>,
    TS::IntoIter: Clone,
{
    let mut iter = tokens.into_iter();
    Program::from_tokens(&mut iter).and_then(|p| match iter.next() {
        Some((token, span)) => Err(ParseError::ExtraToken {
            actual: token,
            span,
        }),
        None => Ok(p),
    })
}
