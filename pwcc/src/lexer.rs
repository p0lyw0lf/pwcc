use core::concat;
use core::fmt::Debug;
use core::str::FromStr;

use miette::Diagnostic;
use regex::Regex;
use regex::RegexSet;
use thiserror::Error;

use crate::span::Span;

mod macros;
use macros::*;

tokens! {
Tokenizer for Token with TokenError:
    r"break\b": KeywordBreak,
    r"case\b": KeywordCase,
    r"continue\b": KeywordContinue,
    r"default\b": KeywordDefault,
    r"do\b": KeywordDo,
    r"else\b": KeywordElse,
    r"for\b": KeywordFor,
    r"goto\b": KeywordGoto,
    r"if\b": KeywordIf,
    r"int\b": KeywordInt,
    r"return\b": KeywordReturn,
    r"switch\b": KeywordSwitch,
    r"void\b": KeywordVoid,
    r"while\b": KeywordWhile,
    r"[a-zA-Z_]\w*\b": Ident(String),
    r"[0-9]+\b": Constant(isize),
    r"!=": NotEqual,
    r"%=": PercentEqual,
    r"&&": DoubleAmpersand,
    r"&=": AmpersandEqual,
    r"--": Decrement,
    r"-=": MinusEqual,
    r"/=": ForwardSlashEqual,
    r"<<": LeftShift,
    r"<<=": LeftShiftEqual,
    r"<=": LessThanEqual,
    r"==": DoubleEqual,
    r">=": GreaterThanEqual,
    r">>": RightShift,
    r">>=": RightShiftEqual,
    r"\*=": StarEqual,
    r"\+=": PlusEqual,
    r"\+\+": Increment,
    r"\^=": CaretEqual,
    r"\|=": PipeEqual,
    r"\|\|": DoublePipe,
    r"!": Exclamation,
    r"%": Percent,
    r"&": Ampersand,
    r",": Comma,
    r"-": Minus,
    r"/": ForwardSlash,
    r":": Colon,
    r";": Semicolon,
    r"<": LessThan,
    r"=": Equal,
    r">": GreaterThan,
    r"\(": OpenParen,
    r"\)": CloseParen,
    r"\*": Star,
    r"\+": Plus,
    r"\?": Question,
    r"\^": Caret,
    r"\{": OpenBrace,
    r"\|": Pipe,
    r"\}": CloseBrace,
    r"~": Tilde,
}

#[derive(Error, Diagnostic, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum LexError {
    #[error("Invalid token")]
    #[diagnostic()]
    InvalidToken {
        #[label("here")]
        span: (usize, usize),
    },
    #[error(transparent)]
    #[diagnostic(transparent)]
    TokenError(#[from] TokenError),
}

/// Lexes a source file into a list of tokens.
pub fn lex(mut source: &str) -> Result<Vec<(Token, Span)>, LexError> {
    let tokenizer = Tokenizer::new();
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

        let (token, len) = tokenizer.consume_token(source, offset)?;
        out.push((token, (offset, len).into()));
        source = &source[len..];
        offset += len;
    }

    Ok(out)
}

#[cfg(test)]
mod test {
    use super::lex as super_lex;
    use super::*;

    fn lex(source: &str) -> Result<Vec<Token>, LexError> {
        let tokens = super_lex(source)?;
        Ok(tokens.into_iter().map(|s| s.0).collect())
    }

    #[test]
    fn extra_paren() {
        use Token::*;
        assert_eq!(
            Ok(vec![OpenParen, Constant(3), CloseParen, CloseParen]),
            lex("(3))")
        );
    }

    #[test]
    fn regex_special_chars() {
        use Token::*;
        assert_eq!(Ok(vec![Plus, Star, Caret, DoublePipe, Pipe]), lex("+*^|||"));
    }
}
