macro_rules! expect_token {
    ($ts:ident, $span:ident, $token:ident$(($pat:pat) : $ty:ty)?) => {
        {
            use Token::*;
            (match $ts.next() {
                None => Err(ParseError::MissingToken {
                    expected: $token$((<$ty as Default>::default().into()))?,
                }),
                Some(Span { inner: $token$((v @ $pat))?, span }) => {
                    $span = $span.sconcat(span);
                    Ok(($(Span { inner: <$ty>::from(v), span })?))
                }
                Some(t) => Err(ParseError::UnexpectedToken {
                    expected: $token$((<$ty as Default>::default().into()))?,
                    actual: t,
                }),
            })?
        }
    };
}
pub(super) use expect_token;

/// Tries multiple functions, returning the when the first returns OK, otherwise returning a
/// default value.
macro_rules! try_parse {
    ($ts:ident, $default:expr, $(
        |$iter:ident| $tt:tt ,
    )*) => {
        {
            $(
            let mut $iter = $ts.clone();
            if let Ok(out) = (|| -> Result<_, ParseError> $tt)() {
                *$ts = $iter;
                return Ok(out);
            }
            )*
            $default
        }
    };
}
pub(super) use try_parse;

/// This is an extremely nasty macro. Unfortunately, it's borne out of necessity: we need to be
/// able to generate all struct definitions at once if we want to use `#[functional_macros::ast]`
/// on them.
macro_rules! nodes {
    ($(
        $node:ident
        $(
            // Multiplication: concatenate all nodes
            ( $(
                *
                $($m_token:ident)?
                $(<$m_sname:ident : $m_subnode:ty>)?
                $({$m_cname:ident : $m_ctoken:ident ($m_pat:pat = $m_ty:ty) })?
            )* )
        )?
        $(
            // Addition: choose between nodes
            ( $(
                +
                $($a_token:ident)?
                $(<$a_subnode:ident>)?
                $({$a_ctoken:ident ($a_pat:pat = $a_ty:ty) })?
            )* )
        )?
        $(
            // Other enum: just emit normally
            enum $oe_tt:tt
        )?
        $(
            // Other struct: just emit normally
            struct $os_tt:tt
        )?
    ; )*) => {
        #[functional_macros::ast]
        mod ast {
        use crate::span::Span;
        use crate::span::SourceSpan;
        use crate::lexer::Token;
        use crate::parser::FromTokens;
        use crate::parser::ToTokens;
        use crate::parser::ParseError;
        use crate::parser::expect_token;
        use crate::parser::try_parse;
        $(
        $(
        // Multiplication
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub struct $node {$(
            $(pub $m_sname: Span<$m_subnode>,)?
            $(pub $m_cname: Span<$m_ty>,)?
        )*}

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Span<Token>> + Clone)) -> Result<Span<Self>, ParseError> {
                fn run(ts: &mut (impl Iterator<Item = Span<Token>> + Clone)) -> Result<Span<$node>, ParseError> {
                    let mut span = SourceSpan::empty();
                    $(
                        $(expect_token!(ts, span, $m_token);)?
                        $(
                            let $m_sname: Span<$m_subnode> = FromTokens::from_tokens(ts)?;
                            span = span.sconcat($m_sname.span);
                        )?
                        $(let $m_cname = expect_token!(ts, span, $m_ctoken($m_pat): $m_ty);)?
                    )*
                    Ok(Span {
                        inner: $node {$(
                            $($m_sname,)?
                            $($m_cname,)?
                        )*},
                        span,
                    })
                }

                run(ts).map_err(|e| ParseError::Context {
                    node_name: stringify!($node).to_string(),
                    err: Box::new(e),
                })
            }
        }

        impl ToTokens for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                let $node {$(
                    $($m_sname,)?
                    $($m_cname,)?
                )*} = self;

                ::core::iter::empty()
                $(.chain(
                    $(::core::iter::once(Token::$m_token))?
                    $($m_sname.inner.to_tokens())?
                    $(::core::iter::once(Token::$m_ctoken($m_cname.inner.into())))?
                ))*
            }
        }
        )?

        $(
        // Addition
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub enum $node {$(
            $($a_token,)?
            $($a_subnode($a_subnode),)?
            $($a_ctoken($a_ty),)?
        )*}

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = Span<Token>> + Clone)) -> Result<Span<Self>, ParseError> {
                fn run(ts: &mut (impl Iterator<Item = Span<Token>> + Clone)) -> Result<Span<$node>, ParseError> {
                    try_parse!(
                        ts,
                        Err(ParseError::NoMatches),
                        $(|iter| {
                            $(
                                let mut span = SourceSpan::empty();
                                expect_token!(iter, span, $a_token);
                                let out = $node::$a_token;
                            )?
                            $(
                                let Span { inner, span } = $a_subnode::from_tokens(&mut iter)?;
                                let out = $node::$a_subnode(inner);
                            )?
                            $(
                                let mut span = SourceSpan::empty();
                                let out = $node::$a_ctoken(expect_token!(iter, span, $a_ctoken($a_pat): $a_ty));
                            )?
                            Ok(Span {
                                inner: out,
                                span,
                            })
                        },)*
                    )
                }

                run(ts).map_err(|e| ParseError::Context {
                    node_name: stringify!($node).to_string(),
                    err: Box::new(e),
                })
            }
        }

        impl ToTokens for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                use $node::*;
                let out: Box<dyn Iterator<Item = Token>> = match self {$(
                    $($a_token => Box::new(::core::iter::once(Token::$a_token)),)?
                    $($a_subnode(s) => Box::new(s.to_tokens()),)?
                    $($a_ctoken(c) => Box::new(::core::iter::once(Token::$a_ctoken(c.into()))),)?
                )*};
                out
            }
        }
        )?
        $(
        // Other enum
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub enum $node $oe_tt
        )?
        $(
        // Other struct
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub struct $node $os_tt;
        )?
        )*
        }
        pub use ast::*;
    };
}
pub(super) use nodes;
