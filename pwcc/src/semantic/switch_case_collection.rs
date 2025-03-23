use miette::Diagnostic;
use thiserror::Error;

use crate::parser::visit_mut;
use crate::parser::visit_mut::VisitMut;
use crate::parser::CaseLabel;
use crate::parser::Function;
use crate::parser::SwitchContext;
use crate::parser::SwitchLabel;
use crate::parser::SwitchStmt;
use crate::semantic::SemanticErrors;
use crate::semantic::UniqueLabelFactory;
use crate::span::Span;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("case {0} is outside of any enclosing switch body")]
    CaseOutsideSwitch(#[label] Span<String>),

    // TODO: I need to get spans on these somehow...
    #[error("\"default\" is outside of any enclosing switch body")]
    DefaultOutsideSwitch,

    #[error("encountered more than one \"default\" inside switch")]
    MoreThanOneDefault,
}

pub(super) fn collect(mut function: Function) -> Result<Function, SemanticErrors> {
    let mut collector = Collector {
        function: function.name.inner.clone(),
        factory: UniqueLabelFactory::default(),
        labels: Default::default(),
        has_default: false,
        errs: Default::default(),
    };

    collector.visit_mut_function(&mut function);

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
    labels: Vec<SwitchLabel>,
    /// Whether we've already seen a `default` case
    has_default: bool,

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
        let mut labels = vec![];
        let mut has_default = false;
        core::mem::swap(&mut self.labels, &mut labels);
        core::mem::swap(&mut self.has_default, &mut has_default);

        // Recur, collecting all the labels for case/default labels belonging to this switch
        visit_mut::visit_mut_switch_stmt(self, switch_stmt);

        // Restore context for outer switch
        core::mem::swap(&mut self.labels, &mut labels);
        core::mem::swap(&mut self.has_default, &mut has_default);

        // Use the newly-collected labels to fill out the context
        switch_stmt.ctx.inner = Some(SwitchContext(labels));
    }

    fn visit_mut_case_label(&mut self, existing_case_label: &mut CaseLabel) {
        // Store explicitly-labeled version into AST, taking ownership of the Exp
        let label = self.make_label();
        let mut case_label = CaseLabel::Labeled(label.clone());
        core::mem::swap(&mut case_label, existing_case_label);

        match case_label {
            CaseLabel::Case(exp) => {
                self.labels.push(SwitchLabel(label, Some(exp)));
            }
            CaseLabel::Default => {
                if self.has_default {
                    self.errs.push(Error::MoreThanOneDefault);
                } else {
                    self.labels.push(SwitchLabel(label, None));
                    self.has_default = true;
                }
            }
            CaseLabel::Labeled(existing) => panic!("case label had existing label {existing}"),
        }
    }
}
