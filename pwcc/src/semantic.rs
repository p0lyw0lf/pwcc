use std::{collections::HashMap, fmt::Display};

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
    let p = todo!("get TryFunctor working"); // TryFunctor::<Declaration>::try_fmap(p, &mut |decl| -> Result<_, SemanticError> { Ok(decl) })?;
    Ok(p)
}

#[derive(Debug)]
pub enum SemanticError {}

impl Display for SemanticError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}
