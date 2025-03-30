use miette::Diagnostic;
use thiserror::Error;

use crate::parser::Exp;
use crate::parser::PostfixOp;
use crate::parser::PrefixOp;
use crate::parser::UnaryOp;
use crate::printer::printable;
use crate::semantic::SemanticErrors;
use crate::span::Span;
use crate::span::Spanned;

pub(super) fn check_operator_types(exp: Exp) -> Result<Exp, SemanticErrors> {
    match exp {
        Exp::Unary {
            op:
                op @ (UnaryOp::PrefixOp(PrefixOp::Increment(_) | PrefixOp::Decrement(_))
                | UnaryOp::PostfixOp(PostfixOp::Increment(_) | PostfixOp::Decrement(_))),
            exp,
            span,
        } => {
            if matches!(*exp, Exp::Var { .. }) {
                Ok(Exp::Unary { op, exp, span })
            } else {
                return Err(Error::InvalidUnaryOp {
                    op_span: op.span(),
                    op: format!("{}", printable(op)),
                    exp_span: exp.span(),
                })?;
            }
        }
        otherwise => Ok(otherwise),
    }
}

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Invalid operand {op} applied to expression")]
    InvalidUnaryOp {
        op: String,
        #[label("op")]
        op_span: Span,
        #[label("expression")]
        exp_span: Span,
    },
}
