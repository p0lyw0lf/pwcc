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
use crate::parser::SwitchStmt;
use crate::parser::WhileStmt;
use crate::semantic::SemanticErrors;
use crate::semantic::UniqueLabelFactory;
use crate::span::Span;
use crate::span::Spanned;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("\"break\" is outside of any enclosing loop or switch body")]
    BreakOutsideLoop(#[label("here")] Span),

    #[error("\"continue\" is outside of any enclosing loop body")]
    ContinueOutsideLoop(#[label("here")] Span),
}

pub(super) fn labeling(mut function: Function) -> Result<Function, SemanticErrors> {
    let mut labeler = Labeler {
        function: function.name.0.clone(),
        break_stack: Default::default(),
        continue_stack: Default::default(),
        factory: UniqueLabelFactory::default(),
        errs: Default::default(),
    };

    labeler.visit_mut_function(&mut function);

    if labeler.errs.len() > 0 {
        Err(SemanticErrors(
            labeler.errs.into_iter().map(Into::into).collect(),
        ))
    } else {
        Ok(function)
    }
}

struct Labeler {
    function: String,
    /// The end of the Vec is the top of the stack. `break`s and `continue`s are treated differently,
    /// because `break` can be used to exit a `switch` while `continue` cannot, and would exit the
    /// nearest loop otherwise.
    break_stack: Vec<String>,
    continue_stack: Vec<String>,
    factory: UniqueLabelFactory,

    errs: Vec<Error>,
}

impl Labeler {
    fn make_label(&mut self) -> LoopLabel {
        LoopLabel(self.factory.unique_label(&self.function))
    }
}

macro_rules! label_loop {
    ($self:expr, $v:expr, $f:ident) => {
        let new_label = $self.make_label();
        $self.break_stack.push(new_label.0.clone());
        $self.continue_stack.push(new_label.0.clone());
        $v.label = Some(new_label);
        visit_mut::$f($self, $v);
        $self
            .break_stack
            .pop()
            .expect("unexpected empty break label stack");
        $self
            .continue_stack
            .pop()
            .expect("unexpected empty break label stack");
    };
}

impl visit_mut::VisitMut for Labeler {
    fn visit_mut_for_stmt(&mut self, for_stmt: &mut ForStmt) {
        label_loop!(self, for_stmt, visit_mut_for_stmt);
    }
    fn visit_mut_while_stmt(&mut self, while_stmt: &mut WhileStmt) {
        label_loop!(self, while_stmt, visit_mut_while_stmt);
    }
    fn visit_mut_do_while_stmt(&mut self, do_while_stmt: &mut DoWhileStmt) {
        label_loop!(self, do_while_stmt, visit_mut_do_while_stmt);
    }

    fn visit_mut_switch_stmt(&mut self, switch_stmt: &mut SwitchStmt) {
        let new_label = self.make_label();
        self.break_stack.push(new_label.0.clone());
        switch_stmt.label= Some(new_label);
        visit_mut::visit_mut_switch_stmt(self, switch_stmt);
        self.break_stack
            .pop()
            .expect("unexpected empty break label stack");
    }

    fn visit_mut_break_stmt(&mut self, break_stmt: &mut BreakStmt) {
        if let Some(label) = self.break_stack.last() {
            break_stmt.label= Some(LoopLabel(label.clone()));
        } else {
            self.errs.push(Error::BreakOutsideLoop(break_stmt.span()));
        }

        visit_mut::visit_mut_break_stmt(self, break_stmt);
    }

    fn visit_mut_continue_stmt(&mut self, continue_stmt: &mut ContinueStmt) {
        if let Some(label) = self.continue_stack.last() {
            continue_stmt.label= Some(LoopLabel(label.clone()));
        } else {
            self.errs.push(Error::ContinueOutsideLoop(continue_stmt.span()));
        }

        visit_mut::visit_mut_continue_stmt(self, continue_stmt);
    }
}
