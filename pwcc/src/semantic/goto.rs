use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use functional::TryFunctor;

use crate::parser::Function;
use crate::parser::GotoStmt;
use crate::parser::RawLabel;
use crate::semantic::SemanticErrors;
use crate::span::Span;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Duplicate label: {label}")]
    DuplicateLabel {
        label: String,
        #[label("first label here")]
        first: Span,
        #[label("duplicated here")]
        second: Span,
    },

    #[error("Missing label for goto: {label}")]
    MissingLabel {
        label: String,
        #[label("here")]
        span: Span,
    },
}

pub fn analysis(f: Function) -> Result<Function, SemanticErrors> {
    let mut labels = HashMap::<String, (String, Span)>::new();

    // First, check for duplicate labels
    let f = f.try_fmap(|raw_label: RawLabel| -> Result<_, SemanticErrors> {
        let (label, span) = raw_label.label;
        match labels.get(&label) {
            Some((_, existing_span)) => {
                return Err(Error::DuplicateLabel {
                    label,
                    first: *existing_span,
                    second: span,
                })?;
            }
            None => {
                labels.insert(label.clone(), (label.clone(), span));
                Ok(RawLabel { label: (label, span), span })
            }
        }
    })?;

    // Then, once we've collected all the labels, check for any missing labels.
    let f = f.try_fmap(|goto_stmt: GotoStmt| -> Result<_, SemanticErrors> {
        if labels.contains_key(&goto_stmt.label.0) {
            Ok(goto_stmt)
        } else {
            return Err(Error::MissingLabel {
                label: goto_stmt.label.0,
                span: goto_stmt.label.1,
            })?;
        }
    })?;

    Ok(f)
}
