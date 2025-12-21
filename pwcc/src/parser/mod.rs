use std::collections::BTreeMap;
use std::iter::once;

use functional::Semigroup;
use pwcc_util::parse_choices;
use pwcc_util::parse_multiple;
use pwcc_util::parse_plus;
use pwcc_util::parse_times;
use pwcc_util::parse_token;
use pwcc_util::parser::FromTokens;
use pwcc_util::parser::ToTokens;
use pwcc_util::parser::as_cloneable;
use pwcc_util::span::Span;
use pwcc_util::span::Spanned;
use pwcc_util::spanned_empty;

use crate::lexer::Token;
use crate::parser::helpers::CommaDelimited;

mod helpers;

pub type ParseError = pwcc_util::parser::error::ParseError<Token>;

#[cfg(test)]
mod test;
#[cfg(test)]
mod test_errors;

// TODO: I probably should make it so the enums are not so huge sometimes...
#[functional_macros::ast(typeclasses = [Functor, TryFunctor, VisitMut])]
mod ast {
    use super::*;

    #[derive(Debug, Spanned)]
    pub struct Program {
        pub functions: Vec<FunctionDecl>,
        pub span: Span,
    }

    #[include()]
    #[derive(Debug, Spanned)]
    pub struct Block {
        pub items: Vec<BlockItem>,
        pub span: Span,
    }

    #[include()]
    #[derive(Debug, Spanned)]
    pub enum BlockItem {
        Statement(Statement),
        Declaration(Declaration),
    }

    #[include()]
    #[derive(Debug, Spanned)]
    pub enum Statement {
        ReturnStmt(ReturnStmt),
        ExpressionStmt(ExpressionStmt),
        IfStmt(IfStmt),
        BreakStmt(BreakStmt),
        ContinueStmt(ContinueStmt),
        WhileStmt(WhileStmt),
        DoWhileStmt(DoWhileStmt),
        ForStmt(ForStmt),
        SwitchStmt(SwitchStmt),
        Block(Block),
        LabelStmt(LabelStmt),
        GotoStmt(GotoStmt),
        NullStmt(NullStmt),
    }

    #[derive(Debug, Spanned)]
    pub struct ReturnStmt {
        pub exp: Exp,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub struct ExpressionStmt {
        pub exp: Exp,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub struct IfStmt {
        pub guard: Exp,
        pub body_true: Box<Statement>,
        pub body_false: Option<Box<Statement>>,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub struct SwitchStmt {
        pub ctx: Option<SwitchContext>,
        pub label: Option<LoopLabel>,
        pub case: Exp,
        pub body: Box<Statement>,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub struct BreakStmt {
        pub label: Option<LoopLabel>,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub struct ContinueStmt {
        pub label: Option<LoopLabel>,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub struct WhileStmt {
        pub label: Option<LoopLabel>,
        pub guard: Exp,
        pub body: Box<Statement>,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub struct DoWhileStmt {
        pub body: Box<Statement>,
        pub label: Option<LoopLabel>,
        pub guard: Exp,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub struct ForStmt {
        pub label: Option<LoopLabel>,
        pub init: ForInit,
        pub exp1: Option<Exp>,
        pub exp2: Option<Exp>,
        pub body: Box<Statement>,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub enum ForInit {
        Decl(VarDecl),
        Exp(Option<Exp>),
    }

    #[derive(Debug, Spanned)]
    pub struct LabelStmt {
        pub label: Label,
        pub stmt: Box<Statement>,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub enum Label {
        Raw(RawLabel),
        Case(CaseLabel),
    }

    #[derive(Debug, Spanned)]
    pub struct RawLabel {
        pub label: (String, Span),
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub struct GotoStmt {
        pub label: (String, Span),
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub struct NullStmt {
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub enum Declaration {
        Function(FunctionDecl),
        Var(VarDecl),
    }

    #[derive(Debug, Spanned)]
    #[include()]
    pub struct FunctionDecl {
        pub name: (String, Span),
        pub args: FunctionDeclArgs,
        pub body: FunctionBody,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub enum FunctionDeclArgs {
        Void(Span),
        DeclArgs(CommaDelimited<DeclArg>),
    }

    #[derive(Debug, Spanned)]
    pub struct DeclArg {
        pub name: (String, Span),
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub enum FunctionBody {
        Declared(Span),
        Defined(Block),
    }

    #[derive(Debug, Spanned)]
    pub struct VarDecl {
        pub name: (String, Span),
        pub init: Initializer,
        pub span: Span,
    }

    #[derive(Debug, Spanned)]
    pub enum Initializer {
        Declared(Span),
        Defined(Exp, Span),
    }

    #[derive(Debug, Spanned)]
    pub enum PrefixOp {
        Minus(Span),
        BitNot(Span),
        LogicNot(Span),
        Increment(Span),
        Decrement(Span),
    }

    #[derive(Debug, Spanned)]
    pub enum PostfixOp {
        Increment(Span),
        Decrement(Span),
    }

    #[derive(Debug, Spanned)]
    pub enum UnaryOp {
        Prefix(PrefixOp),
        Postfix(PostfixOp),
    }

    #[derive(Debug, Spanned)]
    pub enum BinaryOp {
        // Arithmetic operators
        Plus(Span),
        Minus(Span),
        Times(Span),
        Divide(Span),
        Mod(Span),
        // Bitwise operators
        LeftShift(Span),
        RightShift(Span),
        BitAnd(Span),
        BitOr(Span),
        BitXor(Span),
        // Logical operators
        LogicAnd(Span),
        LogicOr(Span),
        Equal(Span),
        NotEqual(Span),
        LessThan(Span),
        LessThanEqual(Span),
        GreaterThan(Span),
        GreaterThanEqual(Span),
    }

    #[derive(Debug, Spanned)]
    pub enum AssignmentOp {
        // Standard assignment
        Equal(Span),
        // Arithmetic assignment
        PlusEqual(Span),
        MinusEqual(Span),
        TimesEqual(Span),
        DivideEqual(Span),
        ModEqual(Span),
        // Bitwise assignment
        LeftShiftEqual(Span),
        RightShiftEqual(Span),
        BitAndEqual(Span),
        BitOrEqual(Span),
        BitXorEqual(Span),
    }

    #[derive(Debug, Spanned)]
    pub enum BinaryTok {
        BinaryOp(BinaryOp),
        AssignmentOp(AssignmentOp),
        TernaryQuestion(Span),
    }

    /// Exp is special, since its AST doesn't exactly correspond with the grammar, so we define it
    /// separately
    #[derive(Debug, Spanned)]
    #[include()]
    pub enum Exp {
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
    }

    // Similarly for LoopLabel; we don't want to be able to parse them, but we do want to be able
    // to represent them.
    #[derive(Debug)]
    pub struct LoopLabel(pub String);

    #[derive(Debug, Spanned)]
    pub enum CaseLabel {
        Case(Exp),
        Default(Span),
        Labeled(String, Span),
    }
    // Maps the constant-evaluated case value (if applicable) to the case it should jump to if the
    // value matches.
    #[derive(Debug)]
    pub struct SwitchContext(pub BTreeMap<Option<isize>, (String, Span)>);
}
pub use ast::*;

/// Helper to define nodes that should not be parsed normally.
macro_rules! unparseable {
    ($node: ident) => {
        impl FromTokens<Token, ParseError> for $node {
            fn from_tokens<'a>(
                _ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
            ) -> Result<Self, ParseError>
            where
                Token: 'a,
            {
                Err(ParseError::NoMatches)
            }
        }
    };
}
parse_times!(Program: *<functions: Vec<FunctionDecl>>);
parse_times!(Block: *OpenBrace *<items: Vec<BlockItem>> *CloseBrace);
parse_plus!(BlockItem:
    +<Statement>: Statement,
    +<Declaration>: Declaration,
);
parse_plus!(Statement:
    +<ReturnStmt>: ReturnStmt,
    +<ExpressionStmt>: ExpressionStmt,
    +<IfStmt>: IfStmt,
    +<BreakStmt>: BreakStmt,
    +<ContinueStmt>: ContinueStmt,
    +<WhileStmt>: WhileStmt,
    +<DoWhileStmt>: DoWhileStmt,
    +<ForStmt>: ForStmt,
    +<SwitchStmt>: SwitchStmt,
    +<Block>: Block,
    +<LabelStmt>: LabelStmt,
    +<GotoStmt>: GotoStmt,
    +<NullStmt>: NullStmt,
);
parse_times!(ReturnStmt: *KeywordReturn *<exp: Exp> *Semicolon);
parse_times!(ExpressionStmt: *<exp: Exp> *Semicolon);

impl FromTokens<Token, ParseError> for IfStmt {
    fn from_tokens<'a>(
        ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
    ) -> Result<Self, ParseError>
    where
        Token: 'a,
    {
        let mut span = Span::empty();
        parse_multiple!(ts, span, {
            *KeywordIf *OpenParen *<guard: Exp> *CloseParen *<body_true: Box<Statement>>
        });

        let mut iter = ts.clone();
        let body_false = (|| -> Result<_, ParseError> {
            parse_multiple!(&mut iter, span, {
                *KeywordElse *<body_false: Box<Statement>>
            });
            *ts = iter;
            Ok(body_false)
        })()
        .ok();

        Ok(IfStmt {
            guard,
            body_true,
            body_false,
            span,
        })
    }
}

impl ToTokens<Token> for IfStmt {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        use Token::*;
        let IfStmt {
            guard,
            body_true,
            body_false,
            span: _,
        } = self;
        once(KeywordIf)
            .chain(once(OpenParen))
            .chain(guard.to_tokens())
            .chain(once(CloseParen))
            .chain(body_true.to_tokens())
            .chain({
                let out: Box<dyn Iterator<Item = Token>> = match body_false {
                    Some(body) => Box::new(once(KeywordElse).chain(body.to_tokens())),
                    None => Box::new(std::iter::empty()),
                };
                out
            })
    }
}

parse_times!(SwitchStmt: *KeywordSwitch *<ctx: Option<SwitchContext>> *<label: Option<LoopLabel>> *OpenParen *<case: Exp> *CloseParen *<body: Box<Statement>>);
parse_times!(BreakStmt: *KeywordBreak *<label: Option<LoopLabel>> *Semicolon);
parse_times!(ContinueStmt: *KeywordContinue *<label: Option<LoopLabel>> *Semicolon);
parse_times!(WhileStmt: *KeywordWhile *<label: Option<LoopLabel>> *OpenParen *<guard: Exp> *CloseParen *<body: Box<Statement>>);
parse_times!(DoWhileStmt: *KeywordDo *<body: Box<Statement>> *KeywordWhile *<label: Option<LoopLabel>> *OpenParen *<guard: Exp> *CloseParen *Semicolon);
parse_times!(ForStmt: *KeywordFor *<label: Option<LoopLabel>> *OpenParen *<init: ForInit> *<exp1: Option<Exp>> *Semicolon *<exp2: Option<Exp>> *CloseParen *<body: Box<Statement>>);

impl FromTokens<Token, ParseError> for ForInit {
    fn from_tokens<'a>(
        ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
    ) -> Result<Self, ParseError>
    where
        Token: 'a,
    {
        parse_choices!(
            ts,
            Err(ParseError::NoMatches),
            |iter| {
                let decl = VarDecl::from_tokens(&mut iter)?;
                Ok(ForInit::Decl(decl))
            },
            |iter| {
                let mut _span = Span::empty();
                parse_multiple!(&mut iter, _span, {
                    *<exp: Option<Exp>> *Semicolon
                });
                Ok(ForInit::Exp(exp))
            },
        )
    }
}

impl ToTokens<Token> for ForInit {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        let out: Box<dyn Iterator<Item = Token>> = match self {
            ForInit::Decl(decl) => Box::new(decl.to_tokens()),
            ForInit::Exp(exp) => Box::new(exp.to_tokens().chain(once(Token::Semicolon))),
        };
        out
    }
}

parse_times!(LabelStmt: *<label: Label> *Colon *<stmt: Box<Statement>>);
parse_plus!(Label:
    +<RawLabel>: Raw,
    +<CaseLabel>: Case,
);
parse_times!(RawLabel: *{label: Ident(_ = String)});

parse_times!(GotoStmt: *KeywordGoto *{label: Ident(_ = String)} *Semicolon);

parse_times!(NullStmt: *Semicolon);

parse_plus!(Declaration:
    +<FunctionDecl>: Function,
    +<VarDecl>: Var,
);
parse_times!(FunctionDecl:
    *KeywordInt *{name: Ident(_ = String)} *OpenParen *<args: FunctionDeclArgs> *CloseParen
        *<body: FunctionBody>
);

parse_plus!(FunctionDeclArgs:
    +KeywordVoid: Void,
    +<CommaDelimited<DeclArg>>: DeclArgs,
);

impl FunctionDeclArgs {
    pub fn num_args(&self) -> usize {
        match self {
            FunctionDeclArgs::Void(_) => 0,
            FunctionDeclArgs::DeclArgs(args) => args.0.len(),
        }
    }
}

impl IntoIterator for FunctionDeclArgs {
    type Item = DeclArg;
    type IntoIter = <Vec<Self::Item> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            FunctionDeclArgs::Void(_) => vec![].into_iter(),
            FunctionDeclArgs::DeclArgs(args) => args.0.into_iter(),
        }
    }
}

impl FunctionDeclArgs {
    pub fn iter(&self) -> std::slice::Iter<'_, DeclArg> {
        match self {
            FunctionDeclArgs::Void(_) => [].iter(),
            FunctionDeclArgs::DeclArgs(args) => args.0.iter(),
        }
    }
}

parse_times!(DeclArg: *KeywordInt *{name: Ident(_ = String)});
parse_plus!(FunctionBody:
    +Semicolon: Declared,
    +<Block>: Defined,
);
parse_times!(VarDecl: *KeywordInt *{name: Ident(_ = String)} *<init: Initializer>);

impl FromTokens<Token, ParseError> for Initializer {
    fn from_tokens<'a>(
        ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
    ) -> Result<Self, ParseError>
    where
        Token: 'a,
    {
        parse_choices!(
            ts,
            Err(ParseError::NoMatches),
            |iter| {
                let mut span = Span::empty();
                parse_token!(iter, span, Semicolon)?;
                Ok(Initializer::Declared(span))
            },
            |iter| {
                let mut span = Span::empty();
                parse_multiple!(&mut iter, span, {
                    *Equal *<exp: Exp> *Semicolon
                });
                Ok(Initializer::Defined(exp, span))
            },
        )
    }
}

impl ToTokens<Token> for Initializer {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        use Token::*;
        let out: Box<dyn Iterator<Item = Token>> = match self {
            Initializer::Declared(_) => Box::new(once(Semicolon)),
            Initializer::Defined(exp, _) => {
                Box::new(once(Equal).chain(exp.to_tokens()).chain(once(Semicolon)))
            }
        };
        out
    }
}

parse_plus!(PrefixOp:
    +Minus: Minus,
    +Tilde: BitNot,
    +Exclamation: LogicNot,
    +Increment: Increment,
    +Decrement: Decrement,
);
parse_plus!(PostfixOp:
    +Increment: Increment,
    +Decrement: Decrement,
);
parse_plus!(UnaryOp:
    +<PrefixOp>: Prefix,
    +<PostfixOp>: Postfix,
);
parse_plus!(BinaryOp:
    // Arithmetic operators
    +Plus: Plus,
    +Minus: Minus,
    +Star: Times,
    +ForwardSlash: Divide,
    +Percent: Mod,
    // Bitwise operators
    +LeftShift: LeftShift,
    +RightShift: RightShift,
    +Ampersand: BitAnd,
    +Pipe: BitOr,
    +Caret: BitXor,
    // Logical operators
    +DoubleAmpersand: LogicAnd,
    +DoublePipe: LogicOr,
    +DoubleEqual: Equal,
    +NotEqual: NotEqual,
    +LessThan: LessThan,
    +LessThanEqual: LessThanEqual,
    +GreaterThan: GreaterThan,
    +GreaterThanEqual: GreaterThanEqual,
);
parse_plus!(AssignmentOp:
    // Standard assignment
    +Equal: Equal,
    // Arithmetic assignment
    +PlusEqual: PlusEqual,
    +MinusEqual: MinusEqual,
    +StarEqual: TimesEqual,
    +ForwardSlashEqual: DivideEqual,
    +PercentEqual: ModEqual,
    // Bitwise assignment
    +LeftShiftEqual: LeftShiftEqual,
    +RightShiftEqual: RightShiftEqual,
    +AmpersandEqual: BitAndEqual,
    +PipeEqual: BitOrEqual,
    +CaretEqual: BitXorEqual,
);
// Not used in grammar, only for parsing Exp
parse_plus!(BinaryTok:
    +<BinaryOp>: BinaryOp,
    +<AssignmentOp>: AssignmentOp,
    // Not a "true" binary operator, but we do parse it like one. This will result in trying to
// parse a ternary expression.
    +Question: TernaryQuestion,
);

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
            TimesEqual(span) => BinaryOp::Times(span),
            DivideEqual(span) => BinaryOp::Divide(span),
            ModEqual(span) => BinaryOp::Mod(span),
            LeftShiftEqual(span) => BinaryOp::LeftShift(span),
            RightShiftEqual(span) => BinaryOp::RightShift(span),
            BitAndEqual(span) => BinaryOp::BitAnd(span),
            BitOrEqual(span) => BinaryOp::BitOr(span),
            BitXorEqual(span) => BinaryOp::BitXor(span),
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
                Times(_) | Divide(_) | Mod(_) => Precedence::Multiplication,
                Plus(_) | Minus(_) => Precedence::Addition,
                LeftShift(_) | RightShift(_) => Precedence::Shift,
                LessThan(_) | LessThanEqual(_) | GreaterThan(_) | GreaterThanEqual(_) => {
                    Precedence::Compare
                }
                Equal(_) | NotEqual(_) => Precedence::Equal,
                BitAnd(_) => Precedence::BitwiseAnd,
                BitOr(_) => Precedence::BitwiseOr,
                BitXor(_) => Precedence::BitwiseXor,
                LogicAnd(_) => Precedence::LogicalAnd,
                LogicOr(_) => Precedence::LogicalOr,
            },
            BinaryTok::TernaryQuestion(_) => Precedence::Ternary,
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

impl FromTokens<Token, ParseError> for Exp {
    fn from_tokens<'a>(
        ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
    ) -> Result<Self, ParseError>
    where
        Token: 'a,
    {
        fn parse_primary<'a>(
            ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
        ) -> Result<Exp, ParseError> {
            parse_choices!(
                ts,
                Err(ParseError::NoMatches),
                |iter| {
                    let mut span = Span::empty();
                    let (constant, _) = parse_token!(iter, span, Constant(_): isize)?;
                    Ok(Exp::Constant { constant, span })
                },
                |iter| {
                    let mut span = Span::empty();
                    parse_multiple!(&mut iter, span, {
                        *{ident: Ident(_ = String)}
                        *OpenParen
                        *<args: CommaDelimited<Exp>>
                        *CloseParen
                    });

                    Ok(Exp::FunctionCall { ident, args, span })
                },
                |iter| {
                    let mut span = Span::empty();
                    let (ident, _) = parse_token!(iter, span, Ident(_): String)?;
                    Ok(Exp::Var { ident, span })
                },
                |iter| {
                    let mut _span = Span::empty();
                    parse_token!(iter, _span, OpenParen)?;
                    let exp = parse_exp(&mut iter, Precedence::lowest())?;
                    parse_token!(iter, _span, CloseParen)?;
                    // Don't include parenthesis in expression span
                    Ok(exp)
                },
            )
        }

        fn parse_postfix<'a>(
            ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
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
                    op: UnaryOp::Postfix(op),
                    exp: Box::new(exp),
                    span,
                };

                peek_iter = iter.clone();
                next_token = PostfixOp::from_tokens(&mut peek_iter);
            }

            *ts = iter;
            Ok(exp)
        }

        fn parse_unary<'a>(
            ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
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
                        op: UnaryOp::Prefix(prefix),
                        exp: Box::new(exp),
                        span,
                    })
                },
                |iter| { parse_postfix(&mut iter) },
            )
        }

        fn parse_exp<'a>(
            ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
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
                    BinaryTok::TernaryQuestion(_) => {
                        let middle = parse_exp(&mut iter, Precedence::lowest())?;
                        parse_token!(iter, span, Colon)?;
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

impl ToTokens<Token> for Exp {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        use Exp::*;
        use Token::*;
        let out: Box<dyn Iterator<Item = Token>> = match self {
            Exp::Constant {
                constant,
                span: _span,
            } => Box::new(once(Token::Constant(constant))),
            Var { ident, span: _span } => Box::new(once(Ident(ident))),
            Unary {
                op,
                exp,
                span: _span,
            } => match op {
                UnaryOp::Prefix(op) => Box::new(
                    op.to_tokens()
                        .chain(once(OpenParen))
                        .chain(exp.to_tokens())
                        .chain(once(CloseParen)),
                ),
                UnaryOp::Postfix(op) => Box::new(
                    once(OpenParen)
                        .chain(exp.to_tokens())
                        .chain(once(CloseParen))
                        .chain(op.to_tokens()),
                ),
            },
            Binary {
                lhs,
                op,
                rhs,
                span: _span,
            } => Box::new(
                once(OpenParen)
                    .chain(lhs.to_tokens())
                    .chain(once(CloseParen))
                    .chain(op.to_tokens())
                    .chain(once(OpenParen))
                    .chain(rhs.to_tokens())
                    .chain(once(CloseParen)),
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
                once(OpenParen)
                    .chain(condition.to_tokens())
                    .chain([CloseParen, Question, OpenParen])
                    .chain(true_case.to_tokens())
                    .chain([CloseParen, Colon, OpenParen])
                    .chain(false_case.to_tokens())
                    .chain(once(CloseParen)),
            ),
            FunctionCall {
                ident,
                args,
                span: _span,
            } => Box::new(
                once(Ident(ident.0))
                    .chain(once(OpenParen))
                    .chain(args.to_tokens())
                    .chain(once(CloseParen)),
            ),
        };
        out
    }
}

spanned_empty!(LoopLabel);
unparseable!(LoopLabel);
impl ToTokens<Token> for LoopLabel {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        once(Token::Ident(self.0))
    }
}

impl FromTokens<Token, ParseError> for CaseLabel {
    fn from_tokens<'a>(
        ts: &mut impl pwcc_util::parser::CloneableIterator<Item = (&'a Token, Span)>,
    ) -> Result<Self, ParseError>
    where
        Token: 'a,
    {
        parse_choices!(
            ts,
            Err(ParseError::NoMatches),
            |iter| {
                let mut _span = Span::empty();
                parse_token!(iter, _span, KeywordCase)?;
                let exp = Exp::from_tokens(&mut iter)?;
                Ok(CaseLabel::Case(exp))
            },
            |iter| {
                let mut span = Span::empty();
                parse_token!(iter, span, KeywordDefault)?;
                Ok(CaseLabel::Default(span))
            },
        )
    }
}

impl ToTokens<Token> for CaseLabel {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        let out: Box<dyn Iterator<Item = Token>> = match self {
            CaseLabel::Case(exp) => Box::new(once(Token::KeywordCase).chain(exp.to_tokens())),
            CaseLabel::Default(_) => Box::new(once(Token::KeywordDefault)),
            CaseLabel::Labeled(label, _) => Box::new(once(Token::Ident(label))),
        };
        out
    }
}

spanned_empty!(SwitchContext);
unparseable!(SwitchContext);
impl ToTokens<Token> for SwitchContext {
    fn to_tokens(self) -> impl Iterator<Item = Token> {
        core::iter::empty()
    }
}

pub fn parse(tokens: &Vec<(Token, Span)>) -> Result<Program, ParseError> {
    let mut ts = as_cloneable(tokens);
    Program::from_tokens(&mut ts).and_then(|p| match ts.next() {
        Some((token, span)) => Err(ParseError::ExtraToken {
            actual: token.clone(),
            span,
        }),
        None => Ok(p),
    })
}
