macro_rules! expect_token {
    ($ts:ident, $span:ident, $token:ident$(($pat:pat) : $ty:ty)?) => {
        {
            use Token::*;
            (match $ts.next() {
                None => Err(ParseError::MissingToken {
                    expected: $token$((<$ty as Default>::default().into()))?,
                }),
                Some(($token$((v @ $pat))?, span)) => {
                    $span = $span.sconcat(span);
                    Ok(($(<$ty>::from(v), span)?))
                }
                Some((t, span)) => Err(ParseError::UnexpectedToken {
                    expected: $token$((<$ty as Default>::default().into()))?,
                    actual: t,
                    span,
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
            $([$m_include:ident])?
        )?
        $(
            // Addition: choose between nodes
            ( $(
                +
                $($a_token:ident)?
                $(<$a_subnode:ident>)?
                $({$a_ctoken:ident ($a_pat:pat = $a_ty:ty) })?
            )* )
            $([$a_include:ident])?
        )?
        $(
            // Other enum: just emit normally
            enum $oe_tt:tt
            $([$oe_include:ident])?
        )?
        $(
            // Other struct: just emit normally
            struct $os_tt:tt
            $([$os_include:ident])?
        )?
    ; )*) => {
        #[functional_macros::ast(typeclasses = [Functor, TryFunctor, VisitMut])]
        mod ast {
        use std::collections::BTreeMap;
        use crate::span::Span;
        use crate::span::Spanned;
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
        $(#[$m_include()])?
        pub struct $node {$(
            $(pub $m_sname: $m_subnode,)?
            $(pub $m_cname: ($m_ty, Span),)?
        )*
            pub span: Span,
        }

        impl Spanned for $node {
            fn span(&self) -> Span {
                self.span
            }
        }

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = (Token, Span)> + Clone)) -> Result<Self, ParseError> {
                fn run(ts: &mut (impl Iterator<Item = (Token, Span)> + Clone)) -> Result<$node, ParseError> {
                    let mut span = Span::empty();
                    $(
                        $(expect_token!(ts, span, $m_token);)?
                        $(
                            let $m_sname: $m_subnode = FromTokens::from_tokens(ts)?;
                            span = span.sconcat($m_sname.span());
                        )?
                        $(let $m_cname = expect_token!(ts, span, $m_ctoken($m_pat): $m_ty);)?
                    )*
                    Ok($node {$(
                        $($m_sname,)?
                        $($m_cname,)?
                    )*
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
                )*
                    ..
                } = self;

                ::core::iter::empty()
                $(.chain(
                    $(::core::iter::once(Token::$m_token))?
                    $($m_sname.to_tokens())?
                    $(::core::iter::once(Token::$m_ctoken($m_cname.0.into())))?
                ))*
            }
        }
        )?

        $(
        // Addition
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        $(#[$a_include()])?
        pub enum $node {$(
            $($a_token(Span),)?
            $($a_subnode($a_subnode),)?
            $($a_ctoken($a_ty, Span),)?
        )*}

        impl Spanned for $node {
            fn span(&self) -> Span {
                match self {$(
                    $($node::$a_token(span) => *span,)?
                    $($node::$a_subnode(node) => node.span(),)?
                    $($node::$a_ctoken(_, span) => *span,)?
                )*}
            }
        }

        impl FromTokens for $node {
            fn from_tokens(ts: &mut (impl Iterator<Item = (Token, Span)> + Clone)) -> Result<Self, ParseError> {
                fn run(ts: &mut (impl Iterator<Item = (Token, Span)> + Clone)) -> Result<$node, ParseError> {
                    try_parse!(
                        ts,
                        Err(ParseError::NoMatches),
                        $(|iter| {
                            $(
                                let mut span = Span::empty();
                                expect_token!(iter, span, $a_token);
                                let out = $node::$a_token(span);
                            )?
                            $(
                                let inner: $a_subnode = FromTokens::from_tokens(&mut iter)?;
                                let out = $node::$a_subnode(inner);
                            )?
                            $(
                                let mut span = Span::empty();
                                let (ty, _) = expect_token!(iter, span, $a_ctoken($a_pat): $a_ty);
                                let out = $node::$a_ctoken(ty, span);
                            )?
                            Ok(out)
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
                    $($a_token(_) => Box::new(::core::iter::once(Token::$a_token)),)?
                    $($a_subnode(s) => Box::new(s.to_tokens()),)?
                    $($a_ctoken(c, _) => Box::new(::core::iter::once(Token::$a_ctoken(c.into()))),)?
                )*};
                out
            }
        }
        )?
        $(
        // Other enum
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        $(#[$oe_include()])?
        pub enum $node $oe_tt
        )?
        $(
        // Other struct
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        $(#[$os_include()])?
        pub struct $node $os_tt;
        )?
        )*
        }
        pub use ast::*;
    };
}
pub(super) use nodes;
