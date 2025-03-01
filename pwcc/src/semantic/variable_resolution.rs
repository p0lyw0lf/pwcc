use miette::Diagnostic;
use std::collections::HashMap;
use thiserror::Error;

use functional::TryFunctor;

use crate::parser::BlockItem;
use crate::parser::Declaration;
use crate::parser::Exp;
use crate::parser::Function;

use crate::semantic::SemanticErrors;
use crate::semantic::UniqueLabelFactory;

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
            return Err(Error::DuplicateDeclaration(name))?;
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
            Exp::Assignment { lhs, op, rhs } => match *lhs {
                Exp::Var { .. } => Ok(Exp::Assignment { lhs, op, rhs }),
                otherwise => Err(Error::InvalidAssignment(otherwise))?,
            },
            Exp::Var { ident } => match self.variable_map.get(&ident) {
                None => Err(Error::UnresolvedVariable(ident))?,
                Some(ident) => Ok(Exp::Var {
                    ident: ident.clone(),
                }),
            },
            otherwise => Ok(otherwise),
        }
    }
}

pub(super) fn resolve_variables(f: Function) -> Result<Function, SemanticErrors> {
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

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Duplicate declaration: {0}")]
    DuplicateDeclaration(String),
    #[error("Unresolved variable: {0}")]
    UnresolvedVariable(String),
    #[error("Cannot assign to: {0:?}")]
    InvalidAssignment(Exp),
}
