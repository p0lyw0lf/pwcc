use std::{collections::HashMap, fmt::Display};

use functional::Functor;

use crate::parser::{Declaration, Exp, Program};

pub fn validate(p: Program) -> Result<Program, SemanticError> {
    let p = resolve_variables(p)?;
    Ok(p)
}

fn resolve_variables(p: Program) -> Result<Program, SemanticError> {
    let mut i: usize = 0;
    // Maps potentially-conflicting names to globally-unique names, in a given context
    let mut variable_map = HashMap::<String, String>::new();
    let p = todo!("implement TryFunctor"); // Functor::<Result<Declaration, SemanticError>>::fmap(p, &mut |decl| Ok(decl))?;
    Ok(p)
}

#[derive(Debug)]
pub enum SemanticError {}

impl Display for SemanticError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}
