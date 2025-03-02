use functional::TryFunctor;
use miette::Diagnostic;
use thiserror::Error;

use crate::parser::Function;
use crate::parser::LoopLabel;
use crate::parser::Statement;
use crate::semantic::SemanticErrors;

use super::UniqueLabelFactory;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    // TODO: I need extra nodes to get this to map over Span<Statement> somehow. I have no idea how
    // to do this lol
    #[error("{0} is outside of any enclosing loop body")]
    OutsideLoop(String),
}

pub(super) fn labeling(function: Function) -> Result<Function, SemanticErrors> {
    let mut labeler = Labeler {
        function: function.name.inner.clone(),
        label_stack: Vec::new(),
        factory: UniqueLabelFactory::default(),
    };

    function.try_fmap_impl(
        &mut |s| labeler.label_statement(s),
        functional::RecursiveCall::None,
    )
}

struct Labeler {
    function: String,
    /// The end of the Vec is the top of the stack
    label_stack: Vec<String>,
    factory: UniqueLabelFactory,
}

impl Labeler {
    fn make_label(&mut self) -> LoopLabel {
        LoopLabel(self.factory.unique_label(&self.function))
    }

    fn label_statement(&mut self, statement: Statement) -> Result<Statement, SemanticErrors> {
        macro_rules! recur {
            ($v:expr) => {
                $v.try_fmap_impl(
                    &mut |s| self.label_statement(s),
                    functional::RecursiveCall::None,
                )?
            };
        }

        macro_rules! label {
            ($v:expr) => {
                let new_label = self.make_label();
                self.label_stack.push(new_label.0.clone());
                $v.label.inner = Some(new_label);
                $v.body = recur!($v.body);
                self.label_stack
                    .pop()
                    .expect("unexpected empty loop label stack");
            };
        }

        Ok(match statement {
            Statement::BreakStmt(mut break_stmt) => {
                if let Some(label) = self.label_stack.last() {
                    break_stmt.label.inner = Some(LoopLabel(label.clone()));
                } else {
                    return Err(Error::OutsideLoop("break".to_string()))?;
                }
                Statement::BreakStmt(break_stmt)
            }
            Statement::ContinueStmt(mut continue_stmt) => {
                if let Some(label) = self.label_stack.last() {
                    continue_stmt.label.inner = Some(LoopLabel(label.clone()));
                } else {
                    return Err(Error::OutsideLoop("continue".to_string()))?;
                }
                Statement::ContinueStmt(continue_stmt)
            }

            Statement::WhileStmt(mut while_stmt) => {
                label!(while_stmt);
                Statement::WhileStmt(while_stmt)
            }
            Statement::DoWhileStmt(mut do_while_stmt) => {
                label!(do_while_stmt);
                Statement::DoWhileStmt(do_while_stmt)
            }
            Statement::ForStmt(mut for_stmt) => {
                label!(for_stmt);
                Statement::ForStmt(for_stmt)
            }

            Statement::IfStmt(if_stmt) => Statement::IfStmt(recur!(if_stmt)),
            Statement::Block(block) => Statement::Block(recur!(block)),

            otherwise @ (Statement::LabelStmt(_)
            | Statement::ReturnStmt(_)
            | Statement::ExpressionStmt(_)
            | Statement::GotoStmt(_)
            | Statement::NullStmt(_)) => otherwise,
        })
    }
}
