/// TODO: document
#[macro_export]
macro_rules! parse_token {
    ($ts:ident, $span:ident, $token:ident$(($pat:pat) : $ty:ty)?) => {
        {
            use Token::*;
            match $ts.next() {
                None => Err($crate::parser::error::ParseError::MissingToken {
                    expected: $token$((<$ty as Default>::default().into()))?.clone(),
                }),
                #[allow(clippy::redundant_pattern)]
                Some(($token$((v @ $pat))?, span)) => {
                    $span.sconcat(span);
                    Ok(($(<$ty>::from(v.clone()), span)?))
                }
                Some((t, span)) => Err($crate::parser::error::ParseError::UnexpectedToken {
                    expected: $token$((<$ty as Default>::default().into()))?.clone(),
                    actual: t.clone(),
                    span,
                }),
            }
        }
    };
}

/// Tries multiple functions, returning the when the first returns OK, otherwise returning a
/// default value.
macro_rules! try_parse {
    ($ts:ident, $default:expr, $(
        |$iter:ident| $tt:tt ,
    )*) => {
        {
            $(
            let mut $iter = $ts.clone();
            let res: Result<_, ParseError> = $tt;
            #[allow(clippy::redundant_closure_call)]
            if let Ok(out) = res {
                *$ts = $iter;
                return Ok(out);
            }
            )*
            $default
        }
    };
}

/// This macro allows you to parse a given node as a "multiplication node", that is, a series of
/// tokens that all must be matched in order for the overall node to match.
///
/// This macro MUST be run with the symbol `Token` in scope. Also, the node it is
/// applied to MUST have exactly the fields specified, and a field `span: Span` for storing the
/// overall span.
///
/// ```
/// use pwcc_util::parse_times;
/// use pwcc_util::parser::as_cloneable;
/// use pwcc_util::lexer::lex;
/// use pwcc_util::Span;
///
/// pwcc_util::tokens! {
/// Tokenizer for Token with TokenError :
///     r"a": A,
///     r"b": B,
///     r"c": C,
///     r"[0-9]+": N(usize),
/// }
///
/// #[derive(Debug, PartialEq)]
/// struct AB {
///     nc: NC,
///     span: Span,
/// }
///
/// #[derive(Debug, PartialEq)]
/// struct NC {
///     n: (usize, Span),
///     span: Span,
/// }
///
/// parse_times!(AB: *A *B *<nc: NC>);
/// parse_times!(NC: *{n: N(_ = usize)} *C);
///
/// impl std::fmt::Display for Token {
///     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
///         match self {
///             Self::A => write!(f, "a"),
///             Self::B => write!(f, "b"),
///             Self::C => write!(f, "c"),
///             Self::N(n) => write!(f, "{n}"),
///         }
///     }
/// }
///
/// // TODO: derive(Spanned)
/// impl pwcc_util::span::Spanned for AB {
///     fn span(&self) -> Span {
///         self.span
///     }
/// }
/// impl pwcc_util::span::Spanned for NC {
///     fn span(&self) -> Span {
///         self.span
///     }
/// }
///
/// let t = Tokenizer::new();
/// let ts = lex("ab2c", |s, i| t.consume_token(s, i)).unwrap();
/// let ts = as_cloneable(&ts);
///
/// let ab: AB = pwcc_util::parser::FromTokens::<Token, _>::from_tokens(&mut ts.clone()).unwrap();
/// assert_eq!(ab, AB {
///     nc: NC {
///         n: (2, (2, 1).into()),
///         span: (2, 2).into(),
///     },
///     span: (0, 4).into(),
/// });
/// assert_eq!(pwcc_util::parser::ToTokens::<Token>::to_tokens(ab).collect::<Vec<_>>(), vec![
///     Token::A,
///     Token::B,
///     Token::N(2),
///     Token::C,
/// ]);
/// ```
///
/// TODO: test cases that ensure the error cases are correct too.
#[macro_export]
macro_rules! parse_times {
    ($node:ident : $(
        *
        $($m_token:ident)?
        $(<$m_sname:ident : $m_subnode:ty>)?
        $({$m_cname:ident : $m_ctoken:ident ($m_pat:pat = $m_ty:ty) })?
    )* ) => {
        impl $crate::parser::FromTokens<Token, $crate::parser::error::ParseError<Token>> for $node {
            fn from_tokens<'a>(
                ts: &mut impl $crate::parser::CloneableIterator<Item = (&'a Token, $crate::span::Span)>,
            ) -> Result<Self, $crate::parser::error::ParseError<Token>>
            where
                Token: 'a,
            {
                fn run<'a>(ts: &mut impl $crate::parser::CloneableIterator<Item = (&'a Token, $crate::span::Span)>) -> Result<$node, $crate::parser::error::ParseError<Token>>
                where
                    Token: 'a,
                {
                    #[allow(unused_import)]
                    use functional::Semigroup;

                    let mut iter = ts.clone();
                    let mut span = Span::empty();
                    $(
                        $($crate::parse_token!(iter, span, $m_token)?;)?
                        $(
                            let $m_sname: $m_subnode = $crate::parser::FromTokens::from_tokens(&mut iter)?;
                            span.sconcat($crate::span::Spanned::span(&$m_sname));
                        )?
                        $(let $m_cname = $crate::parse_token!(iter, span, $m_ctoken($m_pat): $m_ty)?;)?
                    )*
                    *ts = iter;
                    Ok($node {$(
                        $($m_sname,)?
                        $($m_cname,)?
                    )*
                        span,
                    })
                }

                run(ts).map_err(|e| $crate::parser::error::ParseError::Context {
                    node_name: stringify!($node).to_string(),
                    err: Box::new(e),
                })
            }
        }

        impl $crate::parser::ToTokens<Token> for $node {
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
                    $($crate::parser::ToTokens::to_tokens($m_sname))?
                    $(::core::iter::once(Token::$m_ctoken($m_cname.0.into())))?
                ))*
            }
        }
    }
}

/// This is an extremely nasty macro. Unfortunately, it's borne out of necessity: we need to be
/// able to generate all struct definitions at once if we want to use `#[functional_macros::ast]`
/// on them.
macro_rules! nodes {
    (
      { $(use $usepath:path;)* }
      $(
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
        $(use $usepath;)*
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
        )?

        $(
        // Addition
        #[derive(Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        $(#[$a_include()])?
        #[allow(clippy::large_enum_variant)]
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
                                parse_token!(iter, span, $a_token);
                                let out = $node::$a_token(span);
                            )?
                            $(
                                let inner: $a_subnode = FromTokens::from_tokens(&mut iter)?;
                                let out = $node::$a_subnode(inner);
                            )?
                            $(
                                let mut span = Span::empty();
                                let (ty, _) = parse_token!(iter, span, $a_ctoken($a_pat): $a_ty);
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
