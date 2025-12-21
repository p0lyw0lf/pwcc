use miette::Diagnostic;
use thiserror::Error;

use pwcc_util::span::Span;
use pwcc_util::span::Spanned;

use crate::parser::Exp;
use crate::parser::PostfixOp;
use crate::parser::PrefixOp;
use crate::parser::UnaryOp;
use crate::semantic::SemanticErrors;

pub(super) fn check_operator_types(exp: &mut Exp) -> Result<(), SemanticErrors> {
    match exp {
        Exp::Unary {
            op:
                op @ (UnaryOp::Prefix(PrefixOp::Increment(_) | PrefixOp::Decrement(_))
                | UnaryOp::Postfix(PostfixOp::Increment(_) | PostfixOp::Decrement(_))),
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
