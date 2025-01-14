use std::{collections::HashMap, fmt::Display};

use functional::ControlFlow;
use functional::Semigroup;
use functional::TryFunctor;

use crate::parser::Declaration;
use crate::parser::Program;

pub fn validate(p: Program) -> Result<Program, SemanticError> {
    let p = resolve_variables(p)?;
    Ok(p)
}

fn resolve_variables(p: Program) -> Result<Program, SemanticError> {
    let mut i: usize = 0;
    // Maps potentially-conflicting names to globally-unique names, in a given context
    let mut variable_map = HashMap::<String, String>::new();
    let p = TryFunctor::<Declaration>::try_fmap(p, &mut |decl| -> Result<_, SemanticError> {
        Ok(decl)
    })?;
    Ok(p)
}

#[derive(Debug)]
pub enum SemanticError {}

impl Display for SemanticError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl Semigroup for SemanticError {
    fn sconcat(self, other: Self) -> Self {
        // TODO
        other
    }
}

impl ControlFlow for SemanticError {
    fn cont(&self) -> bool {
        true
    }
}
