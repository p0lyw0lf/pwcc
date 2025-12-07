use std::borrow::Borrow;

use miette::Diagnostic;
use thiserror::Error;

use functional::ControlFlow;
use functional::Semigroup;
use functional::TryFunctor;

use crate::parser::FunctionBody;
use crate::parser::FunctionDecl;
use crate::parser::Program;
use crate::parser::visit_mut::VisitMutBuilder;
use crate::parser::visit_mut::VisitMutExt;

mod goto;
mod ident_resolution;
mod loop_labeling;
mod operator_types;
mod switch_case_collection;
mod type_check;

#[cfg(test)]
mod test;

pub use type_check::SymbolTable;

pub fn validate(mut p: Program) -> Result<(Program, SymbolTable), SemanticErrors> {
    let outer_result = {
        let mut visitor = ident_resolution::resolve_idents().chain(type_check::type_check());
        visitor.visit_mut_program(&mut p);

        let (resolution, checker) = visitor.into();
        match (resolution.into(), checker.into()) {
            (Ok(()), Ok(symbol_table)) => Ok(symbol_table),
            (Err(ea), Ok(_)) => Err(ea),
            (Ok(_), Err(eb)) => Err(eb),
            (Err(mut ea), Err(eb)) => {
                ea.sconcat(eb);
                Err(ea)
            }
        }
    };

    let inner_result = p.try_fmap(|f: FunctionDecl| -> Result<_, SemanticErrors> {
        if matches!(f.body, FunctionBody::Semicolon(_)) {
            return Ok(f);
        }

        let function_name = &f.name.0.clone();
        let mut visitor = VisitMutBuilder::visit_mut_exp_pre(operator_types::check_operator_types)
            .chain(VisitMutBuilder::visit_mut_function_decl_pre(goto::analysis))
            .chain(loop_labeling::labeling(function_name))
            .chain(switch_case_collection::collect(function_name));

        let mut f = f;
        visitor.visit_mut_function_decl(&mut f);
        Ok(f)
    });

    match (outer_result, inner_result) {
        (Ok(symbol_table), Ok(p)) => Ok((p, symbol_table)),
        (Err(ea), Ok(_)) => Err(ea),
        (Ok(_), Err(eb)) => Err(eb),
        (Err(mut ea), Err(eb)) => {
            ea.sconcat(eb);
            Err(ea)
        }
    }
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
    IdentResolutionError(#[from] ident_resolution::Error),
    #[error(transparent)]
    #[diagnostic(transparent)]
    TypeCheckError(#[from] type_check::Error),
    #[error(transparent)]
    #[diagnostic(transparent)]
    OperandError(#[from] operator_types::Error),
    #[error(transparent)]
    #[diagnostic(transparent)]
    GotoError(#[from] goto::Error),
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
    fn sconcat(&mut self, other: Self) {
        self.0.sconcat(other.0)
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
