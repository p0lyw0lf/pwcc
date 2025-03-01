use std::collections::HashSet;

use functional::TryFunctor;
use miette::Diagnostic;
use thiserror::Error;

use crate::parser::Block;
use crate::parser::BlockItem;
use crate::parser::Function;
use crate::parser::GotoStmt;
use crate::parser::LabelStmt;
use crate::parser::Statement;
use crate::semantic::SemanticErrors;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Duplicate label: {label}")]
    DuplicateLabel {
        // TODO: once this gets spans, add "first" and "second" fields instead.
        label: String,
    },

    #[error("Missing label for goto: {label}")]
    MissingLabel { label: String },

    #[error("Must have statement after label: {label}")]
    #[help("Try putting a null statement \";\" after the label")]
    NoStatementAfterLabel { label: String },
}

pub fn analysis(f: Function) -> Result<Function, SemanticErrors> {
    let mut labels = HashSet::new();

    // First, check for duplicate labels
    let f = f.try_fmap(&mut |label_stmt: LabelStmt| -> Result<_, SemanticErrors> {
        let is_new = labels.insert(label_stmt.label.clone());
        if is_new {
            Ok(label_stmt)
        } else {
            return Err(Error::DuplicateLabel {
                label: label_stmt.label,
            })?;
        }
    })?;

    // Then, once we've collected all the labels, check for any missing labels.
    let f = f.try_fmap(&mut |goto_stmt: GotoStmt| -> Result<_, SemanticErrors> {
        if labels.contains(&goto_stmt.label) {
            Ok(goto_stmt)
        } else {
            return Err(Error::MissingLabel {
                label: goto_stmt.label,
            })?;
        }
    })?;

    // TODO: make this configurable somehow
    // Check that all labels are followed by a statement, *not* a declaration.
    let f = f.try_fmap(&mut |block: Block| -> Result<_, SemanticErrors> {
        let mut prev_label = None::<&LabelStmt>;
        for item in block.items.iter() {
            if let (Some(label_stmt), BlockItem::Declaration(_)) = (prev_label, item) {
                return Err(Error::NoStatementAfterLabel {
                    label: label_stmt.label.clone(),
                })?;
            }

            if let BlockItem::Statement(Statement::LabelStmt(label_stmt)) = item {
                prev_label = Some(label_stmt);
            } else {
                prev_label = None;
            }
        }

        if let Some(label_stmt) = prev_label {
            return Err(Error::NoStatementAfterLabel {
                label: label_stmt.label.clone(),
            })?;
        }

        Ok(block)
    })?;

    Ok(f)
}
