use std::borrow::Borrow;
use std::fmt::Debug;
use std::fmt::Display;
use std::iter::Iterator;

use miette::Diagnostic;
use miette::LabeledSpan;
use thiserror::Error;

use crate::span::Span;

#[derive(Error, Debug)]
#[error("Parse error")]
pub enum ParseError<Token: Debug + Display> {
    #[error("Unexpected token: expected {expected:-}, got {actual}")]
    UnexpectedToken {
        expected: Token,
        actual: Token,
        span: Span,
    },

    #[error("Missing token: expected {expected}, reached end of tokens")]
    MissingToken { expected: Token },

    #[error("Expected end of tokens, got {actual}")]
    ExtraToken { actual: Token, span: Span },

    #[error("No matches found")]
    NoMatches,

    #[error("While parsing node {node_name}")]
    Context {
        node_name: String,
        // Can't have this be `Box<ParseError<Token>>`, because that results in an infiniite loop
        // of `ParseError<Token>: Error` (outer) only if `ParseError<Token>: Error` (inner, this
        // source). Instead, we are forced to type-erase.
        #[source]
        err: Box<dyn Diagnostic>,
    },
}

#[cfg(test)]
impl<Token: Debug + Display + PartialEq> PartialEq for ParseError<Token> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::UnexpectedToken {
                    expected: l_expected,
                    actual: l_actual,
                    span: l_span,
                },
                Self::UnexpectedToken {
                    expected: r_expected,
                    actual: r_actual,
                    span: r_span,
                },
            ) => l_expected == r_expected && l_actual == r_actual && l_span == r_span,
            (
                Self::MissingToken {
                    expected: l_expected,
                },
                Self::MissingToken {
                    expected: r_expected,
                },
            ) => l_expected == r_expected,
            (
                Self::ExtraToken {
                    actual: l_actual,
                    span: l_span,
                },
                Self::ExtraToken {
                    actual: r_actual,
                    span: r_span,
                },
            ) => l_actual == r_actual && l_span == r_span,
            (
                Self::Context {
                    node_name: l_node_name,
                    err: _l_err,
                },
                Self::Context {
                    node_name: r_node_name,
                    err: _r_err,
                },
            ) => l_node_name == r_node_name,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// miette doesn't seem to like deriving this, so I'll just do it myself ig
impl<Token: Debug + Display> Diagnostic for ParseError<Token> {
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        fn here(span: &Span) -> Box<dyn Iterator<Item = LabeledSpan>> {
            Box::new(core::iter::once(LabeledSpan::new_primary_with_span(
                Some("here".to_string()),
                span,
            )))
        }

        use ParseError::*;
        match self {
            UnexpectedToken { span, .. } => Some(here(span)),
            MissingToken { .. } => None,
            ExtraToken { span, .. } => Some(here(span)),
            NoMatches => None,
            Context { err, .. } => err.labels(),
        }
    }
}

impl<Token: Debug + Display + 'static> Borrow<dyn Diagnostic> for Box<ParseError<Token>> {
    fn borrow(&self) -> &(dyn Diagnostic + 'static) {
        self.as_ref()
    }
}
