use core::fmt::Debug;
use core::iter::Iterator;
use std::borrow::Borrow;

use miette::Diagnostic;
use miette::LabeledSpan;
use thiserror::Error;

use crate::lexer::Token;
use crate::span::Span;

#[derive(Error, Debug)]
#[cfg_attr(test, derive(PartialEq))]
#[error("Parse error")]
pub enum ParseError {
    #[error("Unexpected token: expected {expected:-}, got {actual}")]
    UnexpectedToken {
        expected: Token,
        actual: Span<Token>,
    },

    #[error("Missing token: expected {expected}, reached end of tokens")]
    MissingToken { expected: Token },

    #[error("Expected end of tokens, got {actual}")]
    ExtraToken { actual: Span<Token> },

    #[error("No matches found")]
    NoMatches,

    #[error("While parsing node {node_name}")]
    Context {
        node_name: String,
        #[source]
        err: Box<ParseError>,
    },
}

// miette doesn't seem to like deriving this, so I'll just do it myself ig
impl Diagnostic for ParseError {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        fn here(actual: &Span<Token>) -> Box<dyn Iterator<Item = LabeledSpan>> {
            Box::new(core::iter::once(LabeledSpan::new_primary_with_span(
                Some("here".to_string()),
                actual.span,
            )))
        }

        use ParseError::*;
        match self {
            UnexpectedToken { actual, .. } => Some(here(actual)),
            MissingToken { .. } => None,
            ExtraToken { actual } => Some(here(actual)),
            NoMatches => None,
            Context { err, .. } => err.labels(),
        }
    }
}

impl Borrow<dyn Diagnostic> for Box<ParseError> {
    fn borrow(&self) -> &(dyn Diagnostic + 'static) {
        self.as_ref()
    }
}
