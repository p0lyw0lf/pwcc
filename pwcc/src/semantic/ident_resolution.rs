use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::parser::Block;
use crate::parser::DeclArg;
use crate::parser::Exp;
use crate::parser::ForStmt;
use crate::parser::FunctionBody;
use crate::parser::FunctionDecl;
use crate::parser::Program;
use crate::parser::VarDecl;
use crate::parser::visit_mut;
use crate::parser::visit_mut::VisitMut;
use crate::semantic::SemanticError;
use crate::semantic::SemanticErrors;
use crate::semantic::UniqueLabelFactory;
use crate::span::Span;
use crate::span::Spanned;

#[cfg(test)]
mod test;

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

    #[error("Unresolved ident: {ident}")]
    UnresolvedIdent {
        ident: String,
        #[label("used here")]
        span: Span,
    },

    #[error("Cannot assign to expression")]
    InvalidAssignment(#[label] Span),

    #[error("Nested function definitions are not allowed")]
    LocalFunctionDefinition(#[label] Span),
}

#[derive(Copy, Clone)]
enum Linkage {
    External,
    None,
}

#[derive(Clone)]
struct IdentEntry {
    ident: (String, Span),
    linkage: Linkage,
}

/// Maps potentially-conflicting names to globally-unique names, in a given context
#[derive(Default)]
struct IdentMap {
    // All the mappings created in this scope
    this_scope: HashMap<String, IdentEntry>,
    // The mapping for the parent scope
    parent: Option<Box<IdentMap>>,
    // A cache of the mappings returned by the parent
    parent_cache: HashMap<String, IdentEntry>,
}

impl IdentMap {
    /// When entering a new scope, we should store the current map as the new mapping's pointer
    fn enter_scope(&mut self) {
        let mut tmp = Self {
            this_scope: HashMap::new(),
            parent: None,
            parent_cache: HashMap::new(),
        };

        std::mem::swap(self, &mut tmp);

        // tmp now contains the old self, so insert that as our parent
        self.parent = Some(Box::new(tmp));
    }

    /// When exiting a scope, we should restore from the current map's pointer, because we're going
    /// back to the parent.
    /// MUST be called in exact pairs with enter_scope(), with enter_scope() being called first.
    fn exit_scope(&mut self) {
        let parent = std::mem::take(&mut self.parent).unwrap();
        *self = *parent;
    }

    /// Returns whether the given ident is already declared in the current scope. `Some` contains
    /// the span of the previous declaration.
    fn declared_in_scope(&self, ident: &str) -> Option<&IdentEntry> {
        self.this_scope.get(ident)
    }

    /// Inserts a new mapping into the current scope
    fn new_mapping(
        &mut self,
        uf: &mut UniqueLabelFactory,
        ident: (String, Span),
        linkage: Linkage,
    ) -> String {
        let new_name = match linkage {
            Linkage::External => ident.0.clone(),
            Linkage::None => uf.unique_label(&ident.0),
        };
        self.this_scope.insert(
            ident.0,
            IdentEntry {
                ident: (new_name.clone(), ident.1),
                linkage,
            },
        );
        new_name
    }

    /// Resolves a ident, looking at the current scope first and then any parent scopes
    fn resolve(&mut self, ident: &str) -> Option<IdentEntry> {
        if let Some(name) = self.this_scope.get(ident) {
            return Some(name.clone());
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
struct IdentResolution {
    ident_map: IdentMap,
    /// Keeps a global counter that ensures all varaibles are unique
    factory: UniqueLabelFactory,
    /// The errors collected so far
    errs: Vec<SemanticError>,
}

impl visit_mut::VisitMut for IdentResolution {
    fn visit_mut_var_decl(&mut self, decl: &mut VarDecl) {
        if let Some(existing) = self.ident_map.declared_in_scope(&decl.name.0) {
            self.errs.push(
                Error::DuplicateDeclaration {
                    label: decl.name.0.clone(),
                    first: existing.ident.1,
                    second: decl.name.1,
                }
                .into(),
            );
            return;
        }

        decl.name.0 =
            self.ident_map
                .new_mapping(&mut self.factory, decl.name.clone(), Linkage::None);
        visit_mut::visit_mut_var_decl(self, decl);
    }

    fn visit_mut_function_decl(&mut self, fn_decl: &mut FunctionDecl) {
        if self.ident_map.parent.is_some() && !matches!(fn_decl.body, FunctionBody::Semicolon(_)) {
            self.errs
                .push(Error::LocalFunctionDefinition(fn_decl.span()).into());
            return;
        }

        if let Some(existing) = self.ident_map.declared_in_scope(&fn_decl.name.0)
            && matches!(existing.linkage, Linkage::None)
        {
            self.errs.push(
                Error::DuplicateDeclaration {
                    label: fn_decl.name.0.clone(),
                    first: existing.ident.1,
                    second: fn_decl.name.1,
                }
                .into(),
            );
            return;
        }

        fn_decl.name.0 =
            self.ident_map
                .new_mapping(&mut self.factory, fn_decl.name.clone(), Linkage::External);

        // Can't visit_mut_function_decl directly, since if we do, it will count the arguments and
        // the function body as different scopes. Instead, we manually visit what we need to here.
        self.ident_map.enter_scope();
        visit_mut::visit_mut_function_decl_args(self, &mut fn_decl.args);
        if let FunctionBody::Block(block) = &mut fn_decl.body {
            visit_mut::visit_mut_block(self, block)
        };
        self.ident_map.exit_scope();
    }

    fn visit_mut_decl_arg(&mut self, decl: &mut DeclArg) {
        if let Some(existing) = self.ident_map.declared_in_scope(&decl.name.0) {
            self.errs.push(
                Error::DuplicateDeclaration {
                    label: decl.name.0.clone(),
                    first: existing.ident.1,
                    second: decl.name.1,
                }
                .into(),
            );
            return;
        }

        decl.name.0 =
            self.ident_map
                .new_mapping(&mut self.factory, decl.name.clone(), Linkage::None);
        visit_mut::visit_mut_decl_arg(self, decl);
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
            Exp::Var { ident, span }
            | Exp::FunctionCall {
                ident: (ident, span),
                ..
            } => match self.ident_map.resolve(ident) {
                None => {
                    self.errs.push(
                        Error::UnresolvedIdent {
                            ident: ident.clone(),
                            span: *span,
                        }
                        .into(),
                    );
                }
                Some(new_ident) => {
                    *ident = new_ident.ident.0.clone();
                }
            },
            _ => {}
        };

        visit_mut::visit_mut_exp(self, exp);
    }

    fn visit_mut_for_stmt(&mut self, for_stmt: &mut ForStmt) {
        self.ident_map.enter_scope();
        visit_mut::visit_mut_for_stmt(self, for_stmt);
        self.ident_map.exit_scope();
    }

    fn visit_mut_block(&mut self, block: &mut Block) {
        self.ident_map.enter_scope();
        visit_mut::visit_mut_block(self, block);
        self.ident_map.exit_scope();
    }
}

pub(super) fn resolve_idents(mut p: Program) -> Result<Program, SemanticErrors> {
    let mut ctx = IdentResolution::default();

    ctx.visit_mut_program(&mut p);

    if !ctx.errs.is_empty() {
        Err(SemanticErrors(ctx.errs))
    } else {
        Ok(p)
    }
}
