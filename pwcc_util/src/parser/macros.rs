/// Tries to parse a single next token.
#[macro_export]
macro_rules! parse_token {
    ($ts:ident, $span:ident, $token:ident$(($pat:pat) : $ty:ty)?) => {
        {
            use Token::*;
            use functional::Semigroup;
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
    }
}

/// Tries multiple functions, returning the when the first returns OK, otherwise returning a
/// default value.
#[macro_export]
macro_rules! parse_choices {
    ($ts:ident, $default:expr, $(
        |$iter:ident| $tt:tt ,
    )*) => {
        {
            $(
            let mut $iter = $ts.clone();
            #[allow(clippy::redundant_closure_call)]
            if let Ok(out) = (|| -> Result<_, $crate::parser::error::ParseError<_>> $tt)() {
                *$ts = $iter;
                return Ok(out);
            }
            )*
            $default
        }
    }
}

/// This macro allows you to parse a given node as a "multiplication node", that is, a series of
/// tokens that all must be matched in order for the overall node to match. It generates
/// `FromTokens` and `ToTokens` implementations to accomplish this.
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
/// use pwcc_util::span::Spanned;
///
/// pwcc_util::tokens! {
/// Tokenizer for Token with TokenError :
///     r"a": A,
///     r"b": B,
///     r"c": C,
///     r"[0-9]+": N(usize),
/// }
///
/// #[derive(Debug, PartialEq, Spanned)]
/// struct AB {
///     nc: NC,
///     span: Span,
/// }
///
/// #[derive(Debug, PartialEq, Spanned)]
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
#[macro_export]
macro_rules! parse_times {
    ($node:ident : $(
        *
        $($m_token:ident)?
        $(<$m_sname:ident : $m_subnode:ty>)?
        $({$m_cname:ident : $m_ctoken:ident ($m_pat:pat = $m_ty:ty) })?
        $(,)?
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

/// This macro parsers as given node as an "addition node", that is, choosing between multiple
/// options and parsing as the first successful one.
///
/// This macro MUST be run with the symbol `Token` in scope. Also, the node it is
/// applied to MUST have exactly the case specified, a field `span: Span` for storing the
/// overall span if not in the "subnode" case.
///
/// ```
/// use pwcc_util::parse_plus;
/// use pwcc_util::parser::as_cloneable;
/// use pwcc_util::lexer::lex;
/// use pwcc_util::Span;
/// use pwcc_util::span::Spanned;
///
/// pwcc_util::tokens! {
/// Tokenizer for Token with TokenError :
///     r"a": A,
///     r"b": B,
///     r"[0-9]+": C(usize),
/// }
///
/// #[derive(Debug, PartialEq, Spanned)]
/// enum ABC {
///     A(Span),
///     BC(BC),
/// }
///
/// #[derive(Debug, PartialEq, Spanned)]
/// enum BC {
///     B(Span),
///     C(usize, Span),
/// }
///
/// parse_plus!(
/// ABC:
///     +A: A
///     +<BC>: BC
/// );
/// parse_plus!(
/// BC:
///     +B: B
///     +{C(_ = usize)}: C
/// );
///
/// impl std::fmt::Display for Token {
///     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
///         match self {
///             Self::A => write!(f, "a"),
///             Self::B => write!(f, "b"),
///             Self::C(n) => write!(f, "{n}"),
///         }
///     }
/// }
///
/// let t = Tokenizer::new();
/// let parse = |s: &str| -> ABC {
///     let ts = lex(s, |s, i| t.consume_token(s, i)).unwrap();
///     let mut ts = as_cloneable(&ts);
///     pwcc_util::parser::FromTokens::<Token, _>::from_tokens(&mut ts).unwrap()
/// };
///
/// let s: Span = (0, 1).into();
/// let abc1 = parse("a");
/// assert_eq!(abc1, ABC::A(s));
/// let abc2 = parse("b");
/// assert_eq!(abc2, ABC::BC(BC::B(s)));
/// let abc3 = parse("3");
/// assert_eq!(abc3, ABC::BC(BC::C(3, s)));
/// ```
#[macro_export]
macro_rules! parse_plus {
    ($node:ident : $(
        +
        $($a_token:ident)?
        $(<$a_subnode:ident>)?
        $({$a_ctoken:ident ($a_pat:pat = $a_ty:ty) })?
        : $case:ident $(,)?
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
                    $crate::parse_choices!(
                        ts,
                        Err($crate::parser::error::ParseError::NoMatches),
                        $(|iter| {
                            $(
                                let mut span = $crate::Span::empty();
                                $crate::parse_token!(iter, span, $a_token)?;
                                let out = $node::$case(span);
                            )?
                            $(
                                let inner: $a_subnode = $crate::parser::FromTokens::from_tokens(&mut iter)?;
                                let out = $node::$case(inner);
                            )?
                            $(
                                let mut span = $crate::Span::empty();
                                let (ty, _) = $crate::parse_token!(iter, span, $a_ctoken($a_pat): $a_ty)?;
                                let out = $node::$case(ty, span);
                            )?
                            Ok(out)
                        },)*
                    )
                }

                run(ts).map_err(|e| $crate::parser::error::ParseError::Context {
                    node_name: stringify!($node).to_string(),
                    err: Box::new(e),
                })
            }
        }

        impl $crate::parser::ToTokens<Token> for $node {
            fn to_tokens(self) -> impl Iterator<Item = Token> {
                let out: Box<dyn Iterator<Item = Token>> = match self {$(
                    $($node::$case(_) => Box::new(::core::iter::once(Token::$a_token)),)?
                    $($node::$case(s) => Box::new($a_subnode::to_tokens(s)),)?
                    $($node::$case(c, _) => Box::new(::core::iter::once(Token::$a_ctoken(c.into()))),)?
                )*};
                out
            }
        }
    }
}
