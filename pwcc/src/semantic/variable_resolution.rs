use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::parser::visit_mut;
use crate::parser::visit_mut::VisitMut;
use crate::parser::Block;
use crate::parser::Declaration;
use crate::parser::Exp;
use crate::parser::ForStmt;
use crate::parser::Function;
use crate::semantic::SemanticError;
use crate::semantic::SemanticErrors;
use crate::semantic::UniqueLabelFactory;
use crate::span::Span;
use crate::span::Spanned;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Duplicate declaration: {label}")]
    DuplicateDeclaration {
        label: String,
        #[label("first declared here")]
        first: Span,
        #[label("later declared here")]
        second: Span,
    },

    #[error("Unresolved variable: {variable}")]
    UnresolvedVariable {
        variable: String,
        #[label("used here")]
        span: Span,
    },

    #[error("Cannot assign to expression")]
    InvalidAssignment(#[label] Span),
}

/// Maps potentially-conflicting names to globally-unique names, in a given context
#[derive(Default)]
struct VariableMap {
    // All the mappings created in this block
    this_block: HashMap<String, (String, Span)>,
    // The mapping for the parent block
    parent: Option<Box<VariableMap>>,
    // A cache of the mappings returned by the parent
    parent_cache: HashMap<String, String>,
}

impl VariableMap {
    /// When entering a new block, we should store the current map as the new mapping's pointer
    fn enter_block(&mut self) {
        let mut tmp = Self {
            this_block: HashMap::new(),
            parent: None,
            parent_cache: HashMap::new(),
        };

        std::mem::swap(self, &mut tmp);

        // tmp now contains the old self, so insert that as our parent
        self.parent = Some(Box::new(tmp));
    }

    /// When exiting a block, we should restore from the current map's pointer, because we're going
    /// back to the parent.
    /// MUST be called in exact pairs with enter_block(), with enter_block() being called first.
    fn exit_block(&mut self) {
        let parent = std::mem::take(&mut self.parent).unwrap();
        *self = *parent;
    }

    /// Returns whether the given ident is already declared in the current block. `Some` contains
    /// the span of the previous declaration.
    fn declared_in_block(&self, ident: &str) -> Option<Span> {
        self.this_block.get(ident).map(|d| d.1)
    }

    /// Inserts a new mapping into the current block
    fn new_mapping(&mut self, uf: &mut UniqueLabelFactory, ident: (String, Span)) -> String {
        let unique_name = uf.unique_label(&ident.0);
        self.this_block
            .insert(ident.0, (unique_name.clone(), ident.1));
        unique_name
    }

    /// Resolves a variable
    fn resolve(&mut self, ident: &str) -> Option<String> {
        if let Some(name) = self.this_block.get(ident) {
            return Some(name.0.clone());
        }
        if let Some(name) = self.parent_cache.get(ident) {
            return Some(name.clone());
        }
        if let Some(name) = self
            .parent
            .as_mut()
            .and_then(|parent| parent.resolve(ident))
        {
            self.parent_cache.insert(ident.to_string(), name.clone());
            return Some(name.clone());
        }
        None
    }
}

#[derive(Default)]
struct VariableResolution {
    variable_map: VariableMap,
    /// Keeps a global counter that ensures all varaibles are unique
    factory: UniqueLabelFactory,
    /// The errors collected so far
    errs: Vec<SemanticError>,
}

impl visit_mut::VisitMut for VariableResolution {
    fn visit_mut_declaration(&mut self, decl: &mut Declaration) {
        if let Some(existing) = self.variable_map.declared_in_block(&decl.name.0) {
            self.errs.push(
                Error::DuplicateDeclaration {
                    label: decl.name.0.clone(),
                    first: existing,
                    second: decl.name.1,
                }
                .into(),
            );
            return;
        }

        decl.name.0 = self
            .variable_map
            .new_mapping(&mut self.factory, decl.name.clone());
        visit_mut::visit_mut_declaration(self, decl);
    }

    fn visit_mut_exp(&mut self, exp: &mut Exp) {
        match exp {
            // Make sure all assignments are to Var, and nothing else
            Exp::Assignment { lhs, .. } => match **lhs {
                Exp::Var { .. } => {}
                _ => {
                    self.errs.push(Error::InvalidAssignment(lhs.span()).into());
                }
            },
            // Replace all instantiations of Var with their unique name as determined by the
            // declaration
            Exp::Var { ident, span } => match self.variable_map.resolve(&ident) {
                None => {
                    self.errs
                        .push(Error::UnresolvedVariable {
                            variable: ident.clone(),
                            span: *span,
                        }.into());
                }
                Some(new_ident) => {
                    *ident = new_ident;
                }
            },
            _ => {}
        };

        visit_mut::visit_mut_exp(self, exp);
    }

    fn visit_mut_for_stmt(&mut self, for_stmt: &mut ForStmt) {
        self.variable_map.enter_block();
        visit_mut::visit_mut_for_stmt(self, for_stmt);
        self.variable_map.exit_block();
    }

    fn visit_mut_block(&mut self, block: &mut Block) {
        self.variable_map.enter_block();
        visit_mut::visit_mut_block(self, block);
        self.variable_map.exit_block();
    }
}

pub(super) fn resolve_variables(mut f: Function) -> Result<Function, SemanticErrors> {
    let mut ctx = VariableResolution::default();

    ctx.visit_mut_block(&mut f.body);

    if ctx.errs.len() > 0 {
        Err(SemanticErrors(ctx.errs))
    } else {
        Ok(f)
    }
}
