use std::{collections::HashMap, fmt::Display};

use functional::ControlFlow;
use functional::Semigroup;
use functional::TryFunctor;

use crate::parser::Declaration;
use crate::parser::Exp;
use crate::parser::Function;
use crate::parser::Program;

pub fn validate(p: Program) -> Result<Program, SemanticErrors> {
    let p = TryFunctor::<Function>::try_fmap(p, &mut resolve_variables)?;
    Ok(p)
}

fn resolve_variables(f: Function) -> Result<Function, SemanticErrors> {
    // Maps potentially-conflicting names to globally-unique names, in a given context
    let mut variable_map = HashMap::<String, String>::new();
    // Keeps a global counter that ensures all varaibles are unique
    let mut factory = UniqueLabelFactory::new();

    // Parse all declarations
    let f = TryFunctor::<Declaration>::try_fmap::<SemanticErrors>(f, &mut |decl| {
        let name = decl.name;
        if variable_map.contains_key(&name) {
            return Err(SemanticError::DuplicateDeclaration(name).into());
        }

        let unique_name = factory.unique_label(&name);
        variable_map.insert(name, unique_name.clone());

        Ok(Declaration {
            name: unique_name,
            init: decl.init,
        })
    })?;

    let f = TryFunctor::<Exp>::try_fmap::<SemanticErrors>(f, &mut |exp| match exp {
        Exp::Assignment { lhs, rhs } => match *lhs {
            Exp::Var { .. } => Ok(Exp::Assignment { lhs, rhs }),
            otherwise => Err(SemanticError::InvalidAssignment(otherwise).into()),
        },
        Exp::Var { ident } => match variable_map.get(&ident) {
            None => Err(SemanticError::UnresolvedVariable(ident).into()),
            Some(ident) => Ok(Exp::Var {
                ident: ident.clone(),
            }),
        },
        otherwise => Ok(otherwise),
    })?;

    Ok(f)
}

struct UniqueLabelFactory {
    i: usize,
}

impl UniqueLabelFactory {
    fn new() -> Self {
        Self { i: 0 }
    }
    fn unique_label(&mut self, label: &str) -> String {
        let out = format!("{}.{}", label, self.i);
        self.i += 1;
        out
    }
}

#[derive(Debug)]
pub enum SemanticError {
    DuplicateDeclaration(String),
    UnresolvedVariable(String),
    InvalidAssignment(Exp),
}

pub struct SemanticErrors(pub Vec<SemanticError>);

impl From<SemanticError> for SemanticErrors {
    fn from(value: SemanticError) -> Self {
        Self(Vec::from([value]))
    }
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use SemanticError::*;
        match self {
            DuplicateDeclaration(label) => write!(f, "Duplicate variable declaration: {label}"),
            UnresolvedVariable(label) => write!(f, "Unresolved variable: {label}"),
            // TODO: make this Display instead of Debug
            InvalidAssignment(exp) => write!(f, "Cannot assign to: {exp:?}"),
        }
    }
}

impl Display for SemanticErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for err in self.0.iter() {
            writeln!(f, "{err}")?;
        }
        Ok(())
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
