use std::borrow::Borrow;

use miette::Diagnostic;
use thiserror::Error;

use functional::ControlFlow;
use functional::Semigroup;
use functional::TryFunctor;

use crate::parser::FunctionBody;
use crate::parser::FunctionDecl;
use crate::parser::Program;

mod goto;
mod ident_resolution;
mod loop_labeling;
mod operator_types;
mod switch_case_collection;

pub fn validate(p: Program) -> Result<Program, SemanticErrors> {
    let p = p.try_fmap(ident_resolution::resolve_idents)?;
    let p = p.try_fmap(|f: FunctionDecl| -> Result<_, SemanticErrors> {
        if matches!(f.body, FunctionBody::Semicolon(_)) {
            return Ok(f);
        }
        let f = f.try_fmap(operator_types::check_operator_types)?;
        let f = f.try_fmap(goto::analysis)?;
        let f = f.try_fmap(loop_labeling::labeling)?;
        let f = f.try_fmap(switch_case_collection::collect)?;
        Ok(f)
    })?;
    Ok(p)
}

#[derive(Default)]
struct UniqueLabelFactory {
    i: usize,
}

impl UniqueLabelFactory {
    fn unique_label(&mut self, label: &str) -> String {
        let out = format!("{}.{}", label, self.i);
        self.i += 1;
        out
    }
}

#[derive(Error, Diagnostic, Debug)]
pub enum SemanticError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    OperandError(#[from] operator_types::Error),
    #[error(transparent)]
    #[diagnostic(transparent)]
    GotoError(#[from] goto::Error),
    #[error(transparent)]
    #[diagnostic(transparent)]
    VariableResolutionError(#[from] ident_resolution::Error),
    #[error(transparent)]
    #[diagnostic(transparent)]
    LoopLabelingError(#[from] loop_labeling::Error),
    #[error(transparent)]
    #[diagnostic(transparent)]
    SwitchCaseError(#[from] switch_case_collection::Error),
}

#[derive(Error, Diagnostic, Debug)]
#[error("Semantic errors")]
pub struct SemanticErrors(#[related] pub Vec<SemanticError>);

impl<E: Into<SemanticError>> From<E> for SemanticErrors {
    fn from(value: E) -> Self {
        Self(Vec::from([value.into()]))
    }
}

impl Semigroup for SemanticErrors {
    fn sconcat(self, other: Self) -> Self {
        Self(self.0.sconcat(other.0))
    }
}

impl ControlFlow for SemanticErrors {
    fn cont(&self) -> bool {
        true
    }
}

impl Borrow<dyn Diagnostic> for Box<SemanticErrors> {
    fn borrow(&self) -> &(dyn Diagnostic + 'static) {
        self.as_ref()
    }
}
