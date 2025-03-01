use miette::Diagnostic;
use thiserror::Error;

use crate::parser::Exp;
use crate::parser::PostfixOp;
use crate::parser::PrefixOp;
use crate::parser::UnaryOp;
use crate::semantic::SemanticErrors;

pub(super) fn check_operator_types(exp: Exp) -> Result<Exp, SemanticErrors> {
    match exp {
        Exp::Unary {
            op:
                op @ (UnaryOp::PrefixOp(PrefixOp::Increment | PrefixOp::Decrement)
                | UnaryOp::PostfixOp(PostfixOp::Increment | PostfixOp::Decrement)),
            exp,
        } => {
            if matches!(*exp, Exp::Var { .. }) {
                Ok(Exp::Unary { op, exp })
            } else {
                return Err(Error::InvalidUnaryOp { op, exp: *exp })?;
            }
        }
        otherwise => Ok(otherwise),
    }
}

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    // TODO: make this formatting better
    #[error("Invalid operand {op:?} applied to expression {exp:?}")]
    InvalidUnaryOp { op: UnaryOp, exp: Exp },
}
