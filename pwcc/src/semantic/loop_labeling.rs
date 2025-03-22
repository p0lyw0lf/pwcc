use miette::Diagnostic;
use thiserror::Error;

use crate::parser::visit_mut;
use crate::parser::visit_mut::VisitMut;
use crate::parser::BreakStmt;
use crate::parser::ContinueStmt;
use crate::parser::DoWhileStmt;
use crate::parser::ForStmt;
use crate::parser::Function;
use crate::parser::LoopLabel;
use crate::parser::WhileStmt;
use crate::semantic::SemanticError;
use crate::semantic::SemanticErrors;
use crate::semantic::UniqueLabelFactory;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    // TODO: I need extra nodes to get this to map over Span<Statement> somehow. I have no idea how
    // to do this lol
    #[error("{0} is outside of any enclosing loop body")]
    OutsideLoop(String),
}

pub(super) fn labeling(mut function: Function) -> Result<Function, SemanticErrors> {
    let mut labeler = Labeler {
        function: function.name.inner.clone(),
        label_stack: Vec::new(),
        factory: UniqueLabelFactory::default(),
        errs: Default::default(),
    };

    labeler.visit_mut_function(&mut function);

    if labeler.errs.len() > 0 {
        Err(SemanticErrors(labeler.errs))
    } else {
        Ok(function)
    }
}

struct Labeler {
    function: String,
    /// The end of the Vec is the top of the stack
    label_stack: Vec<String>,
    factory: UniqueLabelFactory,

    errs: Vec<SemanticError>,
}

impl Labeler {
    fn make_label(&mut self) -> LoopLabel {
        LoopLabel(self.factory.unique_label(&self.function))
    }
}

macro_rules! label {
    ($self:expr, $v:expr, $f:ident) => {
        let new_label = $self.make_label();
        $self.label_stack.push(new_label.0.clone());
        $v.label.inner = Some(new_label);
        visit_mut::$f($self, $v);
        $self
            .label_stack
            .pop()
            .expect("unexpected empty loop label stack");
    };
}

impl visit_mut::VisitMut for Labeler {
    fn visit_mut_for_stmt(&mut self, for_stmt: &mut ForStmt) {
        label!(self, for_stmt, visit_mut_for_stmt);
    }
    fn visit_mut_while_stmt(&mut self, while_stmt: &mut WhileStmt) {
        label!(self, while_stmt, visit_mut_while_stmt);
    }
    fn visit_mut_do_while_stmt(&mut self, do_while_stmt: &mut DoWhileStmt) {
        label!(self, do_while_stmt, visit_mut_do_while_stmt);
    }

    fn visit_mut_break_stmt(&mut self, break_stmt: &mut BreakStmt) {
        if let Some(label) = self.label_stack.last() {
            break_stmt.label.inner = Some(LoopLabel(label.clone()));
        } else {
            self.errs
                .push(Error::OutsideLoop("break".to_string()).into());
        }

        visit_mut::visit_mut_break_stmt(self, break_stmt);
    }

    fn visit_mut_continue_stmt(&mut self, continue_stmt: &mut ContinueStmt) {
        if let Some(label) = self.label_stack.last() {
            continue_stmt.label.inner = Some(LoopLabel(label.clone()));
        } else {
            self.errs
                .push(Error::OutsideLoop("continue".to_string()).into());
        }

        visit_mut::visit_mut_continue_stmt(self, continue_stmt);
    }
}
