use core::fmt::Display;

use crate::lexer::LexError;
use crate::parser::ParseError;

impl<'a> Display for LexError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LexError::*;
        match self {
            InvalidToken(t) => write!(f, "invalid token: {t}"),
            TokenError(e) => write!(f, "SHOULD NOT HAPPEN: error parsing token: {e}"),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParseError::*;
        match self {
            UnexpectedToken { expected, actual } => write!(
                f,
                "unexpected token: expected {:-}, got {}",
                expected, actual
            ),
            MissingToken { expected } => write!(f, "missing token: expected {:-}", expected),
            ExtraToken { actual } => {
                write!(f, "extra token: expected end of stream, got {}", actual)
            }
            NoMatches => {
                // TODO: make this more informative ?
                write!(f, "did not match any branches")
            }
        }
    }
}
