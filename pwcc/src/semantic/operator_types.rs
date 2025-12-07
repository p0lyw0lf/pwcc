use miette::Diagnostic;
use thiserror::Error;

use crate::parser::Exp;
use crate::parser::PostfixOp;
use crate::parser::PrefixOp;
use crate::parser::UnaryOp;
use crate::semantic::SemanticErrors;
use crate::span::Span;
use crate::span::Spanned;

pub(super) fn check_operator_types(exp: &mut Exp) -> Result<(), SemanticErrors> {
    match exp {
        Exp::Unary {
            op:
                op @ (UnaryOp::PrefixOp(PrefixOp::Increment(_) | PrefixOp::Decrement(_))
                | UnaryOp::PostfixOp(PostfixOp::Increment(_) | PostfixOp::Decrement(_))),
            exp,
            span: _span,
        } => {
            if matches!(&**exp, Exp::Var { .. }) {
                Ok(())
            } else {
                Err(Error::InvalidUnaryOp {
                    // TODO: find some way to serialize `op` to string without having to take
                    // ownership
                    op_span: op.span(),
                    exp_span: exp.span(),
                })?
            }
        }
        _ => Ok(()),
    }
}

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Invalid operand applied to expression")]
    InvalidUnaryOp {
        #[label("op")]
        op_span: Span,
        #[label("expression")]
        exp_span: Span,
    },
}
