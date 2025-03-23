use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use functional::TryFunctor;

use crate::parser::Block;
use crate::parser::BlockItem;
use crate::parser::Function;
use crate::parser::GotoStmt;
use crate::parser::LabelStmt;
use crate::parser::RawLabel;
use crate::parser::Statement;
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

    #[error("Must have statement after label")]
    #[help("Try putting a null statement \";\" after the label")]
    NoStatementAfterLabel(#[label("here")] SourceSpan),
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

    // TODO: make this configurable somehow, later C standards allow this
    // Check that all labels are followed by a statement, *not* a declaration.
    let f = f.try_fmap(|block: Block| -> Result<_, SemanticErrors> {
        let mut prev_label = None::<&LabelStmt>;
        for item in block.items.iter() {
            if let (Some(label_stmt), BlockItem::Declaration(_)) = (prev_label, item) {
                return Err(Error::NoStatementAfterLabel(label_stmt.label.span))?;
            }

            if let BlockItem::Statement(Statement::LabelStmt(label_stmt)) = item {
                prev_label = Some(label_stmt);
            } else {
                prev_label = None;
            }
        }

        if let Some(label_stmt) = prev_label {
            return Err(Error::NoStatementAfterLabel(label_stmt.label.span))?;
        }

        Ok(block)
    })?;

    Ok(f)
}
