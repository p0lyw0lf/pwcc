macro_rules! tokens {
    ($tokenizer:ident for $tokens:ident with $tokenerror:ident : $(
        $regex:literal : $t:ident $( ( $inner:ty ) )? ,
    )*) => {

        #[derive(Debug, Clone)]
        #[cfg_attr(test, derive(PartialEq))]
        pub enum $tokens {
            $($t$(($inner))?,)*
        }

        #[derive(Error, Diagnostic, Debug)]
        #[cfg_attr(test, derive(PartialEq))]
        pub enum $tokenerror {
            $($(
            #[error("Could not parse token {}: {err}", std::stringify!($t))]
            $t {
                err: <$inner as FromStr>::Err,
                #[label]
                span: (usize, usize),
            },
            )?)*
        }

        struct $tokenizer {
            rs: RegexSet,
            pats: Vec<Regex>,
        }

        /// Converts a string into the `n`th token. MUST only be called if
        /// said string matches the token's regex.
        static TO_TOKEN: &'static [fn (&str, usize) -> Result<$tokens, $tokenerror>] = &[$(
            |_s: &str, _offset: usize| -> Result<$tokens, $tokenerror> {
                Ok($tokens::$t$(({
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
            unsafe fn consume_specific_token(&self, source: &str, source_offset: usize, regex_index: usize) -> Result<($tokens, usize), TokenError> {
                let pat = unsafe { self.pats.get_unchecked(regex_index) };
                let token_str = pat.find_at(source, 0).unwrap().as_str();

                let token = unsafe { TO_TOKEN.get_unchecked(regex_index) }(token_str, source_offset)?;

                Ok((token, token_str.len()))
            }

            /// Given a `source`, return the next token starting at that
            /// offset, and how much to advance the `source_offset` by.
            /// NOTE: `source_offset` is only used for error reporting.
            pub fn consume_token(&self, source: &str, source_offset: usize) -> Result<($tokens, usize), LexError> {
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
                    None => return Err(LexError::InvalidToken {
                        span: (
                            source_offset,
                            safe_truncate(source, 8).len(),
                        ),
                    }),
                };

                Ok(unsafe {
                    self.consume_specific_token(source, source_offset, first_match)
                        .map_err(LexError::TokenError)?
                })
            }
        }
    };
}
pub(super) use tokens;
