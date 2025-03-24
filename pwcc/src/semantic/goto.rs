use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use functional::TryFunctor;

use crate::parser::Function;
use crate::parser::GotoStmt;
use crate::parser::RawLabel;
use crate::semantic::SemanticErrors;
use crate::span::SourceSpan;
use crate::span::Span;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Duplicate label: {second}")]
    DuplicateLabel {
        #[label("first label here")]
        first: SourceSpan,
        #[label("duplicated here")]
        second: Span<String>,
    },

    #[error("Missing label for goto: {0}")]
    MissingLabel(#[label("here")] Span<String>),
}

pub fn analysis(f: Function) -> Result<Function, SemanticErrors> {
    let mut labels = HashMap::<String, SourceSpan>::new();

    // First, check for duplicate labels
    let f = f.try_fmap(|raw_label: RawLabel| -> Result<_, SemanticErrors> {
        match labels.get(&raw_label.label.inner) {
            Some(existing) => {
                return Err(Error::DuplicateLabel {
                    first: existing.clone(),
                    second: raw_label.label.clone(),
                })?;
            }
            None => {
                labels.insert(raw_label.label.inner.clone(), raw_label.label.span);
                Ok(raw_label)
            }
        }
    })?;

    // Then, once we've collected all the labels, check for any missing labels.
    let f = f.try_fmap(|goto_stmt: GotoStmt| -> Result<_, SemanticErrors> {
        if labels.contains_key(&goto_stmt.label.inner) {
            Ok(goto_stmt)
        } else {
            return Err(Error::MissingLabel(goto_stmt.label))?;
        }
    })?;

    Ok(f)
}
