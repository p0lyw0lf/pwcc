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
            #[error("Could not parse token {}: {0}", std::stringify!($t))]
            $t(<$inner as FromStr>::Err),
            )?)*
        }

        struct $tokenizer {
            rs: RegexSet,
            pats: Vec<Regex>,
        }

        /// Converts a string into the `n`th token. MUST only be called if
        /// said string matches the token's regex.
        static TO_TOKEN: &'static [fn (&str) -> Result<$tokens, $tokenerror>] = &[$(
            |_s: &str| -> Result<$tokens, $tokenerror> {
                Ok($tokens::$t$(({
                    let inner = <$inner as FromStr>::from_str(_s).map_err($tokenerror::$t)?;
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
            unsafe fn consume_specific_token<'a>(&self, source: &'a str, index: usize) -> Result<(Token, &'a str), TokenError> {
                let pat = self.pats.get_unchecked(index);
                let token_str = pat.find_at(source, 0).unwrap().as_str();

                let token = TO_TOKEN.get_unchecked(index)(token_str)?;
                let (_, rest) = source.split_at(token_str.len());

                Ok((token, rest))
            }

            pub fn consume_token<'a>(&self, source: &'a str) -> Result<($tokens, &'a str), LexError> {
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
                    None => return Err(LexError::InvalidToken(safe_truncate(source, 8).to_string())),
                };

                Ok(unsafe {
                    self.consume_specific_token(source, first_match)
                        .map_err(LexError::TokenError)?
                })
            }
        }
    };
}
pub(super) use tokens;
