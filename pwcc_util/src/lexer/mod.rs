/// Creates a tokenizer given a set of tokens. Usage:
/// ```
/// pwcc_util::tokens! {
/// Tokenizer for Token with TokenError :
///     r"foo ": Foo,
///     r"bar ": Bar,
///     r"[0-9]+": Literal(usize),
/// }
///
/// let t = Tokenizer::new();
///
/// let s = "foo bar 9 baz";
/// let mut i = 0usize;
///
/// let (t0, len) = t.consume_token(&s[i..], i).unwrap();
/// assert_eq!(t0, Token::Foo);
/// i += len;
///
/// let (t1, len) = t.consume_token(&s[i..], i).unwrap();
/// assert_eq!(t1, Token::Bar);
/// i += len;
///
/// let (t2, len) = t.consume_token(&s[i..], i).unwrap();
/// assert_eq!(t2, Token::Literal(9));
/// i += len;
///
/// let err = t.consume_token(&s[i..], i).unwrap_err();
/// assert!(matches!(err, TokenError::InvalidToken { .. }));
/// ```
///
/// Rules:
/// + The first part is the regex that the token will match against.
/// + Regexes are checked in order, top to bottom.
/// + The token must have zero or one associated items.
/// + If the token has 1 associated item, it must implement FromStr, taking the _entire match_ as
///   input.
/// + Whitespace is not handled automatically, and must be done external to the tokenizer.
#[macro_export]
macro_rules! tokens {
    ($tokenizer:ident for $token:ident with $tokenerror:ident : $(
        $regex:literal : $t:ident $( ( $inner:ty ) )? ,
    )*) => {
        mod tokens {
        use core::concat;
        use core::fmt::Debug;
        use core::str::FromStr;

        use miette::Diagnostic;
        use regex::Regex;
        use regex::RegexSet;
        use thiserror::Error;

        #[derive(Debug, Clone, PartialEq)]
        pub enum $token {
            $($t$(($inner))?,)*
        }

        #[derive(Error, Diagnostic, Debug)]
        #[cfg_attr(any(test, doctest), derive(PartialEq))]
        pub enum $tokenerror {
            #[error("Invalid token")]
            #[diagnostic()]
            InvalidToken {
                #[label("here")]
                span: (usize, usize),
            },
            $($(
            #[error("Could not parse token {}: {err}", std::stringify!($t))]
            $t {
                err: <$inner as FromStr>::Err,
                #[label]
                span: (usize, usize),
            },
            )?)*
        }

        pub struct $tokenizer {
            rs: RegexSet,
            pats: Vec<Regex>,
        }

        /// Converts a string into the `n`th token. MUST only be called if
        /// said string matches the token's regex.
        static TO_TOKEN: &'static [fn (&str, usize) -> Result<$token, $tokenerror>] = &[$(
            |_s: &str, _offset: usize| -> Result<$token, $tokenerror> {
                Ok($token::$t$(({
                    let inner = <$inner as FromStr>::from_str(_s).map_err(
                        |err| $tokenerror::$t {
                            err,
                            span: (_offset, _s.len()),
                        }
                    )?;
                    inner
                }))?)
            },
        )*];

        impl $tokenizer {
            pub fn new() -> Self {
                let rs = RegexSet::new([$(concat!("^", $regex),)*]).expect("invalid regex");
                let pats = rs.patterns().iter().map(|pat| Regex::new(pat).unwrap()).collect::<Vec<_>>();

                return Self {
                    rs,
                    pats,
                }
            }

            /// SAFETY: must only be called when `source` starts with the token
            /// at the specified index
            unsafe fn consume_specific_token(&self, source: &str, source_offset: usize, regex_index: usize) -> Result<($token, usize), $tokenerror> {
                let pat = unsafe { self.pats.get_unchecked(regex_index) };
                let token_str = pat.find_at(source, 0).unwrap().as_str();

                let token = unsafe { TO_TOKEN.get_unchecked(regex_index) }(token_str, source_offset)?;

                Ok((token, token_str.len()))
            }

            /// Given a `source`, return the next token starting at that
            /// offset, and how much to advance the `source_offset` by.
            /// NOTE: `source_offset` is only used for error reporting.
            pub fn consume_token(&self, source: &str, source_offset: usize) -> Result<($token, usize), $tokenerror> {
                // Truncates the string so it appears about `mid` characters long
                fn safe_truncate(s: &str, mut mid: usize) -> &str {
                    while mid < s.len() {
                        if let Some((front, _)) = s.split_at_checked(mid) {
                            return front;
                        }
                        mid += 1;
                    }
                    s
                }

                let first_match = match self.rs.matches_at(source, 0).iter().next() {
                    Some(m) => m,
                    None => return Err($tokenerror::InvalidToken {
                        span: (
                            source_offset,
                            safe_truncate(source, 8).len(),
                        ),
                    }),
                };

                unsafe {
                    self.consume_specific_token(source, source_offset, first_match)
                }
            }
        }
        }
        pub use tokens::*;
    };
}

/// Lexes a source file into a list of tokens, given a function that will consume 1 token. This
/// function trims all whitespace between tokens.
///
/// ```
/// use pwcc_util::lexer::lex;
///
/// pwcc_util::tokens! {
/// Tokenizer for Token with TokenError :
///     r"[0-9]+": Number(usize),
/// }
///
/// let t = Tokenizer::new();
/// let nums = lex("1 2 3", |s, i| t.consume_token(s, i)).unwrap();
/// assert_eq!(
///     vec![
///         Token::Number(1),
///         Token::Number(2),
///         Token::Number(3),
///     ],
///     nums.into_iter().map(|(n, _)| n).collect::<Vec<_>>()
/// );
///
/// let err = lex("foobar", |s, i| t.consume_token(s, i)).unwrap_err();
/// assert!(matches!(err, TokenError::InvalidToken { .. }));
/// ```
pub fn lex<T, E>(
    mut source: &str,
    mut consume_token: impl FnMut(&str, usize) -> Result<(T, usize), E>,
) -> Result<Vec<(T, crate::Span)>, E> {
    let mut out = Vec::new();
    let mut offset = 0;
    while !source.is_empty() {
        let old_len = source.len();
        source = source.trim_start();
        if source.is_empty() {
            break;
        }

        let new_len = source.len();
        offset += old_len - new_len;

        let (token, len) = consume_token(source, offset)?;
        out.push((token, (offset, len).into()));
        source = &source[len..];
        offset += len;
    }

    Ok(out)
}
