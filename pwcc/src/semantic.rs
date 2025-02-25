use std::{collections::HashMap, fmt::Display};

use functional::ControlFlow;
use functional::Semigroup;
use functional::TryFunctor;

use crate::parser::BlockItem;
use crate::parser::Declaration;
use crate::parser::Exp;
use crate::parser::Function;
use crate::parser::Program;

pub fn validate(p: Program) -> Result<Program, SemanticErrors> {
    let p = p.try_fmap(&mut resolve_variables)?;
    Ok(p)
}

#[derive(Default)]
struct VariableResolution {
    /// Maps potentially-conflicting names to globally-unique names, in a given context
    variable_map: HashMap<String, String>,
    /// Keeps a global counter that ensures all varaibles are unique
    factory: UniqueLabelFactory,
}

impl VariableResolution {
    fn resolve_decl(self: &mut Self, decl: Declaration) -> Result<Declaration, SemanticErrors> {
        let name = decl.name;
        if self.variable_map.contains_key(&name) {
            return Err(SemanticError::DuplicateDeclaration(name).into());
        }

        let unique_name = self.factory.unique_label(&name);
        self.variable_map.insert(name.clone(), unique_name.clone());

        Ok(Declaration {
            name: unique_name,
            init: decl.init,
        })
    }

    fn resolve_exp(self: &Self, exp: Exp) -> Result<Exp, SemanticErrors> {
        match exp {
            Exp::Assignment { lhs, rhs } => match *lhs {
                Exp::Var { .. } => Ok(Exp::Assignment { lhs, rhs }),
                otherwise => Err(SemanticError::InvalidAssignment(otherwise).into()),
            },
            Exp::Var { ident } => match self.variable_map.get(&ident) {
                None => Err(SemanticError::UnresolvedVariable(ident).into()),
                Some(ident) => Ok(Exp::Var {
                    ident: ident.clone(),
                }),
            },
            otherwise => Ok(otherwise),
        }
    }
}

fn resolve_variables(f: Function) -> Result<Function, SemanticErrors> {
    let mut ctx = VariableResolution::default();

    // Parse all declarations and statements, in order.
    let f = TryFunctor::<BlockItem>::try_fmap::<SemanticErrors>(f, &mut |item| {
        // This will only apply for Declaration, not for Statement. Neat!
        let item = TryFunctor::<Declaration>::try_fmap(item, &mut |decl| ctx.resolve_decl(decl))?;
        let item = TryFunctor::<Exp>::try_fmap(item, &mut |exp| ctx.resolve_exp(exp))?;
        Ok(item)
    })?;

    Ok(f)
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
