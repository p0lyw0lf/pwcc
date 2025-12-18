use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::parser::GotoStmt;
use crate::parser::RawLabel;
use crate::parser::visit_mut::VisitMut;
use crate::parser::visit_mut::VisitMutBuilder;
use crate::semantic::SemanticError;
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

type Labels = HashMap<String, (String, Span)>;

pub fn collect_duplicates() -> impl VisitMut + Into<Result<Labels, SemanticErrors>> {
    struct Duplicates {
        labels: Labels,
        errs: Vec<SemanticError>,
    }

    impl VisitMut for Duplicates {
        fn visit_mut_raw_label_pre(&mut self, raw_label: &mut RawLabel) {
            let (label, span) = &raw_label.label;
            match self.labels.get(label) {
                Some((_, existing_span)) => {
                    self.errs.push(
                        Error::DuplicateLabel {
                            label: label.to_string(),
                            first: *existing_span,
                            second: *span,
                        }
                        .into(),
                    );
                }
                None => {
                    self.labels.insert(label.clone(), (label.clone(), *span));
                }
            }
        }
    }

    impl Into<Result<Labels, SemanticErrors>> for Duplicates {
        fn into(self) -> Result<Labels, SemanticErrors> {
            if self.errs.len() > 0 {
                Err(SemanticErrors(self.errs))
            } else {
                Ok(self.labels)
            }
        }
    }

    Duplicates {
        labels: Labels::new(),
        errs: Vec::new(),
    }
}

pub fn find_missing(labels: Labels) -> impl VisitMut + Into<Result<(), SemanticErrors>> {
    VisitMutBuilder::visit_mut_goto_stmt_pre(
        move |goto_stmt: &mut GotoStmt| -> Result<(), SemanticErrors> {
            if labels.contains_key(&goto_stmt.label.0) {
                Ok(())
            } else {
                Err(Error::MissingLabel {
                    label: goto_stmt.label.0.clone(),
                    span: goto_stmt.label.1,
                })?
            }
        },
    )
}
