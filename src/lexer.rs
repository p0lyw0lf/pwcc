use core::concat;
use core::fmt::Debug;
use core::str::FromStr;

use regex::Regex;
use regex::RegexSet;

mod macros;
use macros::*;

tokens! {
Tokenizer for Token with TokenError:
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
    r"\+\+": Increment,
    r"--": Decrement,
    r"\+": Plus,
    r"-": Minus,
    r"\*": Star,
    r"/": ForwardSlash,
    r"%": Percent,
    r"~": Tilde,
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum LexError<'a> {
    InvalidToken(&'a str),
    TokenError(TokenError),
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn extra_paren() {
        use Token::*;
        assert_eq!(
            Ok(Vec::from([OpenParen, Constant(3), CloseParen, CloseParen])),
            lex("(3))")
        );
    }
}
