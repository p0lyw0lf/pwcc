use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::parser::FunctionDecl;
use crate::parser::GotoStmt;
use crate::parser::RawLabel;
use crate::parser::visit_mut::VisitMutBuilder;
use crate::parser::visit_mut::VisitMutExt;
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

// TODO: I'd really like to minimize the number of passes done overall, but this does require two
// passes to complete. I should probably break this up, but for now this remains easier.
pub fn analysis(f: &mut FunctionDecl) -> Result<(), SemanticErrors> {
    let mut labels = HashMap::<String, (String, Span)>::new();

    // First, check for duplicate labels
    {
        let mut collect_duplicates = VisitMutBuilder::visit_mut_raw_label_pre(
            |raw_label: &mut RawLabel| -> Result<(), SemanticErrors> {
                let (label, span) = &raw_label.label;
                match labels.get(label) {
                    Some((_, existing_span)) => Err(Error::DuplicateLabel {
                        label: label.to_string(),
                        first: *existing_span,
                        second: *span,
                    })?,
                    None => {
                        labels.insert(label.clone(), (label.clone(), *span));
                        Ok(())
                    }
                }
            },
        );
        collect_duplicates.visit_mut_function_decl(f);
        collect_duplicates.into()
    }?;

    // Then, once we've collected all the labels, check for any missing labels.
    {
        let mut find_missing = VisitMutBuilder::visit_mut_goto_stmt_pre(
            |goto_stmt: &mut GotoStmt| -> Result<(), SemanticErrors> {
                if labels.contains_key(&goto_stmt.label.0) {
                    Ok(())
                } else {
                    Err(Error::MissingLabel {
                        label: goto_stmt.label.0.clone(),
                        span: goto_stmt.label.1,
                    })?
                }
            },
        );
        find_missing.visit_mut_function_decl(f);
        find_missing.into()
    }?;

    Ok(())
}
