use core::concat;
use core::fmt::Debug;
use core::str::FromStr;

use miette::Diagnostic;
use regex::Regex;
use regex::RegexSet;
use thiserror::Error;

mod macros;
use macros::*;

tokens! {
Tokenizer for Token with TokenError:
    r"int\b": KeywordInt,
    r"void\b": KeywordVoid,
    r"return\b": KeywordReturn,
    r"[a-zA-Z_]\w*\b": Ident(String),
    r"[0-9]+\b": Constant(isize),
    r"\+=": PlusEqual,
    r"-=": MinusEqual,
    r"\*=": StarEqual,
    r"/=": ForwardSlashEqual,
    r"%=": PercentEqual,
    r"&=": AmpersandEqual,
    r"\|=": PipeEqual,
    r"\^=": CaretEqual,
    r"<<=": LeftShiftEqual,
    r">>=": RightShiftEqual,
    r"!=": NotEqual,
    r"<=": LessThanEqual,
    r"==": DoubleEqual,
    r">=": GreaterThanEqual,
    r"<<": LeftShift,
    r">>": RightShift,
    r"\+\+": Increment,
    r"--": Decrement,
    r"&&": DoubleAmpersand,
    r"\|\|": DoublePipe,
    r"!": Exclamation,
    r"\+": Plus,
    r"-": Minus,
    r"\*": Star,
    r"/": ForwardSlash,
    r"%": Percent,
    r"<": LessThan,
    r"=": Equal,
    r">": GreaterThan,
    r"&": Ampersand,
    r"\|": Pipe,
    r"\^": Caret,
    r"~": Tilde,
    r"\(": OpenParen,
    r"\)": CloseParen,
    r"\{": OpenBrace,
    r"\}": CloseBrace,
    r";": Semicolon,
}

#[derive(Error, Diagnostic, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum LexError {
    #[error("Invalid token: {0}")]
    InvalidToken(String),
    #[error(transparent)]
    #[diagnostic(transparent)]
    TokenError(#[from] TokenError),
}

/// Lexes a source file into a list of tokens.
pub fn lex(mut source: &str) -> Result<Vec<Token>, LexError> {
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

    #[test]
    fn regex_special_chars() {
        use Token::*;
        assert_eq!(
            Ok(Vec::from([Plus, Star, Caret, DoublePipe, Pipe])),
            lex("+*^|||")
        );
    }
}
