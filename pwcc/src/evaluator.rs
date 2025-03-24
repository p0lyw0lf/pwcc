use miette::Diagnostic;
use thiserror::Error;

use crate::parser::BinaryOp;
use crate::parser::Exp;
use crate::parser::PostfixOp;
use crate::parser::PrefixOp;
use crate::parser::UnaryOp;
use crate::span::SourceSpan;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("variable cannot be used in a constant expression")]
    Variable(#[label("here")] SourceSpan),

    #[error("operator cannot be used in a constant expression")]
    InvalidOperator(#[label("here")] SourceSpan),
}

/// Evaluates a constant expression at compile-time. Returns an error describing why it cannot, if
/// applicable.
pub fn evaluate(exp: Exp) -> Result<isize, Error> {
    match exp {
        Exp::Constant { constant } => Ok(constant),
        Exp::Var { ident } => {
            // Eventually, these might be able to be resolved at compile time, but for now they are
            // not.
            Err(Error::Variable(ident.span))
        }
        Exp::Unary { op, exp } => {
            let v = evaluate(*exp.inner)?;
            match op.inner {
                UnaryOp::PrefixOp(PrefixOp::Minus) => Ok(-v),
                UnaryOp::PrefixOp(PrefixOp::Tilde) => Ok(!v),
                UnaryOp::PrefixOp(PrefixOp::Exclamation) => Ok((v == 0) as isize),
                UnaryOp::PrefixOp(PrefixOp::Increment | PrefixOp::Decrement) => {
                    Err(Error::InvalidOperator(op.span))
                }
                UnaryOp::PostfixOp(PostfixOp::Increment | PostfixOp::Decrement) => {
                    Err(Error::InvalidOperator(op.span))
                }
            }
        }
        Exp::Binary { lhs, op, rhs } => {
            let l = evaluate(*lhs.inner)?;
            let r = evaluate(*rhs.inner)?;
            match op.inner {
                BinaryOp::Plus => Ok(l + r),
                BinaryOp::Minus => Ok(l - r),
                BinaryOp::Star => Ok(l * r),
                BinaryOp::ForwardSlash => Ok(l / r),
                BinaryOp::Percent => Ok(l % r),
                BinaryOp::LeftShift => Ok(l << r),
                BinaryOp::RightShift => Ok(l >> r),
                BinaryOp::Ampersand => Ok(l & r),
                BinaryOp::Caret => Ok(l ^ r),
                BinaryOp::Pipe => Ok(l | r),
                BinaryOp::DoubleAmpersand => Ok((l != 0 && r != 0) as isize),
                BinaryOp::DoublePipe => Ok((l != 0 || r != 0) as isize),
                BinaryOp::DoubleEqual => Ok((l == r) as isize),
                BinaryOp::NotEqual => Ok((l != r) as isize),
                BinaryOp::LessThan => Ok((l < r) as isize),
                BinaryOp::LessThanEqual => Ok((l <= r) as isize),
                BinaryOp::GreaterThan => Ok((l > r) as isize),
                BinaryOp::GreaterThanEqual => Ok((l >= r) as isize),
            }
        }
        Exp::Ternary {
            condition,
            true_case,
            false_case,
        } => {
            let c = evaluate(*condition.inner)?;
            let t = evaluate(*true_case.inner)?;
            let f = evaluate(*false_case.inner)?;
            Ok(if c != 0 { t } else { f })
        }
        Exp::Assignment { op, .. } => Err(Error::InvalidOperator(op.span)),
    }
}
