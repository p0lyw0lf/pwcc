use miette::Diagnostic;
use thiserror::Error;

use crate::parser::Exp;
use crate::parser::PostfixOp;
use crate::parser::PrefixOp;
use crate::parser::UnaryOp;
use crate::printer::printable;
use crate::semantic::SemanticErrors;
use crate::span::SourceSpan;
use crate::span::Span;

pub(super) fn check_operator_types(exp: Exp) -> Result<Exp, SemanticErrors> {
    match exp {
        Exp::Unary {
            op:
                op @ Span {
                    inner:
                        UnaryOp::PrefixOp(PrefixOp::Increment | PrefixOp::Decrement)
                        | UnaryOp::PostfixOp(PostfixOp::Increment | PostfixOp::Decrement),
                    ..
                },
            exp,
        } => {
            if matches!(*exp.inner, Exp::Var { .. }) {
                Ok(Exp::Unary { op, exp })
            } else {
                return Err(Error::InvalidUnaryOp {
                    op: Span {
                        span: op.span,
                        inner: format!("{}", printable(op.inner)),
                    },
                    exp: exp.span,
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
        #[label("op")]
        op: Span<String>,
        #[label("expression")]
        exp: SourceSpan,
    },
}
