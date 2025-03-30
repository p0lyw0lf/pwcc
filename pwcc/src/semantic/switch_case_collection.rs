use std::collections::btree_map;
use std::collections::BTreeMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::evaluator;
use crate::evaluator::evaluate;
use crate::parser::visit_mut;
use crate::parser::visit_mut::VisitMut;
use crate::parser::CaseLabel;
use crate::parser::FunctionDecl;
use crate::parser::SwitchContext;
use crate::parser::SwitchStmt;
use crate::semantic::SemanticErrors;
use crate::semantic::UniqueLabelFactory;
use crate::span::Span;
use crate::span::Spanned;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("\"case\" is outside of any enclosing switch body")]
    CaseOutsideSwitch(#[label("here")] Span),

    #[error("\"default\" is outside of any enclosing switch body")]
    DefaultOutsideSwitch(#[label("here")] Span),

    #[error("encountered more than one \"default\" inside switch")]
    MoreThanOneDefault {
        #[label("first was here")]
        first: Span,
        #[label("later found here")]
        second: Span,
    },

    #[error("\"case\" does not have constant value")]
    #[diagnostic(transparent)]
    NonConstantCase(#[source] evaluator::Error),

    #[error("more than one \"case\" for value {value}")]
    DuplicateCase {
        value: isize,
        #[label("first was here")]
        first: Span,
        #[label("later found here")]
        second: Span,
    },
}

pub(super) fn collect(mut function: FunctionDecl) -> Result<FunctionDecl, SemanticErrors> {
    let mut collector = Collector {
        function: function.name.0.clone(),
        factory: UniqueLabelFactory::default(),
        labels: Default::default(),
        errs: Default::default(),
    };

    collector.visit_mut_function_decl(&mut function);

    if collector.errs.len() > 0 {
        Err(SemanticErrors(
            collector.errs.into_iter().map(Into::into).collect(),
        ))
    } else {
        Ok(function)
    }
}

struct Collector {
    function: String,
    factory: UniqueLabelFactory,
    /// The current switch cases we've seen so far
    labels: Option<BTreeMap<Option<isize>, (String, Span)>>,

    errs: Vec<Error>,
}

impl Collector {
    fn make_label(&mut self) -> String {
        self.factory.unique_label(&self.function)
    }
}

impl visit_mut::VisitMut for Collector {
    fn visit_mut_switch_stmt(&mut self, switch_stmt: &mut SwitchStmt) {
        // Save context for outer switch on the stack
        let mut labels = Some(BTreeMap::new());
        core::mem::swap(&mut self.labels, &mut labels);

        // Recur, collecting all the labels for case/default labels belonging to this switch
        visit_mut::visit_mut_switch_stmt(self, switch_stmt);

        // Restore context for outer switch
        core::mem::swap(&mut self.labels, &mut labels);

        // Use the newly-collected labels to fill out the context
        switch_stmt.ctx = Some(SwitchContext(
            labels.expect("labels became unset while visiting switch cases"),
        ));
    }

    fn visit_mut_case_label(&mut self, existing_case_label: &mut CaseLabel) {
        // Store explicitly-labeled version into AST, taking ownership of the Exp
        let label = self.make_label();
        let mut case_label = CaseLabel::Labeled(label.clone(), existing_case_label.span());
        core::mem::swap(&mut case_label, existing_case_label);

        match case_label {
            CaseLabel::Case(exp) => match self.labels.as_mut() {
                Some(labels) => {
                    let span = exp.span();
                    match evaluate(exp) {
                        Ok(v) => match labels.entry(Some(v)) {
                            btree_map::Entry::Vacant(e) => {
                                e.insert((label, span));
                            }
                            btree_map::Entry::Occupied(e) => {
                                let (_, existing_span) = e.get();
                                self.errs.push(Error::DuplicateCase {
                                    value: v,
                                    first: *existing_span,
                                    second: span,
                                })
                            }
                        },
                        Err(e) => self.errs.push(Error::NonConstantCase(e)),
                    }
                }
                None => self.errs.push(Error::CaseOutsideSwitch(exp.span())),
            },
            CaseLabel::Default(span) => match self.labels.as_mut() {
                Some(labels) => match labels.entry(None) {
                    btree_map::Entry::Vacant(e) => {
                        e.insert((label, span));
                    }
                    btree_map::Entry::Occupied(e) => {
                        let (_, existing_span) = e.get();
                        self.errs.push(Error::MoreThanOneDefault {
                            first: *existing_span,
                            second: span,
                        })
                    }
                },
                None => self.errs.push(Error::DefaultOutsideSwitch(span)),
            },
            CaseLabel::Labeled(existing, _) => panic!("case label had existing label {existing}"),
        }
    }
}
