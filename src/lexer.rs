use core::fmt::Display;

macro_rules! tokens {
    ($($regex:literal : $t:ident $( ( $inner:ty ) )? , )*) => {
        use ::core::concat;
        use ::core::fmt::Debug;
        use ::core::str::FromStr;

        use ::regex::Regex;
        use ::regex::RegexSet;

        #[derive(Clone, Debug, PartialEq, Eq)]
        pub enum Token {
            $($t$(($inner))?,)*
        }

        #[derive(Debug)]
        pub enum TokenError {
            $($($t(<$inner as FromStr>::Err),)?)*
        }

        struct Tokenizer {
            rs: RegexSet,
            pats: Vec<Regex>,
        }

        /// Converts a string into the `n`th token. MUST only be called if
        /// said string matches the token's regex.
        static TO_TOKEN: &'static [fn (&str) -> Result<Token, TokenError>] = &[$(
            |s: &str| -> Result<Token, TokenError> {
                Ok(Token::$t$(({
                    let inner = <$inner as FromStr>::from_str(s).map_err(TokenError::$t)?;
                    inner
                }))?)
            },
        )*];

        impl Tokenizer {
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

                Ok((token, source.trim_start_matches(token_str)))
            }
        }
    };
}

impl Tokenizer {
    pub fn consume_token<'a>(&self, source: &'a str) -> Result<(Token, &'a str), LexError<'a>> {
        let first_match = match self.rs.matches_at(source, 0).iter().next() {
            Some(m) => m,
            None => return Err(LexError::UnexpectedToken(source)),
        };

        Ok(unsafe {
            self.consume_specific_token(source, first_match)
                .map_err(LexError::TokenError)?
        })
    }
}

tokens! {
    r"int\b": KeywordInt,
    r"void\b": KeywordVoid,
    r"return\b": KeywordReturn,
    r"[a-zA-Z_]\w*\b": Ident(String),
    r"[0-9]+\b": Constant(isize),
    r"\(": OpenParen,
    r"\)": CloseParen,
    r"\{": OpenBrace,
    r"\}": CloseBrace,
    r";": Semicolon,
}

#[derive(Debug)]
pub enum LexError<'a> {
    UnexpectedToken(&'a str),
    TokenError(TokenError),
}

impl<'a> Display for LexError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LexError::*;
        match self {
            UnexpectedToken(t) => write!(f, "unexpected token: {t}"),
            TokenError(e) => write!(f, "SHOULD NOT HAPPEN: error parsing token: {e:?}"),
        }
    }
}

/// Lexes a source file into a list of tokens.
pub fn lex(mut source: &str) -> Result<Vec<Token>, LexError<'_>> {
    let tokenizer = Tokenizer::new();
    let mut out = Vec::<Token>::new();
    while !source.is_empty() {
        source = source.trim_start();
        if source.is_empty() {
            break;
        }

        let (token, new_source) = tokenizer.consume_token(source)?;
        out.push(token);
        source = new_source;
    }

    Ok(out)
}
