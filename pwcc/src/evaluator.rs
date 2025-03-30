use miette::Diagnostic;
use thiserror::Error;

use crate::parser::BinaryOp;
use crate::parser::Exp;
use crate::parser::PostfixOp;
use crate::parser::PrefixOp;
use crate::parser::UnaryOp;
use crate::span::Span;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("variable cannot be used in a constant expression")]
    Variable(#[label("here")] Span),

    #[error("operator cannot be used in a constant expression")]
    InvalidOperator(#[label("here")] Span),
}

/// Evaluates a constant expression at compile-time. Returns an error describing why it cannot, if
/// applicable.
pub fn evaluate(exp: Exp) -> Result<isize, Error> {
    match exp {
        Exp::Constant {
            constant,
            span: _span,
        } => Ok(constant),
        Exp::Var { ident: _ident, span } => {
            // Eventually, these might be able to be resolved at compile time, but for now they are
            // not.
            Err(Error::Variable(span))
        }
        Exp::Unary { op, exp, span } => {
            let v = evaluate(*exp)?;
            match op {
                UnaryOp::PrefixOp(PrefixOp::Minus(_)) => Ok(-v),
                UnaryOp::PrefixOp(PrefixOp::Tilde(_)) => Ok(!v),
                UnaryOp::PrefixOp(PrefixOp::Exclamation(_)) => Ok((v == 0) as isize),
                UnaryOp::PrefixOp(PrefixOp::Increment(_) | PrefixOp::Decrement(_)) => {
                    Err(Error::InvalidOperator(span))
                }
                UnaryOp::PostfixOp(PostfixOp::Increment(_) | PostfixOp::Decrement(_)) => {
                    Err(Error::InvalidOperator(span))
                }
            }
        }
        Exp::Binary {
            lhs,
            op,
            rhs,
            span: _span,
        } => {
            let l = evaluate(*lhs)?;
            let r = evaluate(*rhs)?;
            match op {
                BinaryOp::Plus(_) => Ok(l + r),
                BinaryOp::Minus(_) => Ok(l - r),
                BinaryOp::Star(_) => Ok(l * r),
                BinaryOp::ForwardSlash(_) => Ok(l / r),
                BinaryOp::Percent(_) => Ok(l % r),
                BinaryOp::LeftShift(_) => Ok(l << r),
                BinaryOp::RightShift(_) => Ok(l >> r),
                BinaryOp::Ampersand(_) => Ok(l & r),
                BinaryOp::Caret(_) => Ok(l ^ r),
                BinaryOp::Pipe(_) => Ok(l | r),
                BinaryOp::DoubleAmpersand(_) => Ok((l != 0 && r != 0) as isize),
                BinaryOp::DoublePipe(_) => Ok((l != 0 || r != 0) as isize),
                BinaryOp::DoubleEqual(_) => Ok((l == r) as isize),
                BinaryOp::NotEqual(_) => Ok((l != r) as isize),
                BinaryOp::LessThan(_) => Ok((l < r) as isize),
                BinaryOp::LessThanEqual(_) => Ok((l <= r) as isize),
                BinaryOp::GreaterThan(_) => Ok((l > r) as isize),
                BinaryOp::GreaterThanEqual(_) => Ok((l >= r) as isize),
            }
        }
        Exp::Ternary {
            condition,
            true_case,
            false_case,
            span: _span,
        } => {
            let c = evaluate(*condition)?;
            let t = evaluate(*true_case)?;
            let f = evaluate(*false_case)?;
            Ok(if c != 0 { t } else { f })
        }
        Exp::Assignment { span, .. } => Err(Error::InvalidOperator(span)),
    }
}
