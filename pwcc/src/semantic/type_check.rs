use std::collections::HashMap;
use std::fmt::Display;

use miette::Diagnostic;
use thiserror::Error;

use crate::parser::Exp;
use crate::parser::visit_mut::VisitMut;
use crate::semantic::SemanticError;
use crate::semantic::SemanticErrors;
use crate::semantic::ToErrors;
use crate::span::Span;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Int,
    Function(/** parameter count */ usize),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Function(0) => write!(f, "int function(void)"),
            Type::Function(n) => {
                write!(f, "int function(")?;
                for _ in 0..n - 1 {
                    write!(f, "int, ")?;
                }
                write!(f, "int)")
            }
        }
    }
}

pub struct Declaration {
    ty: Type,
    defined: bool,
    span: Span,
}

#[derive(Default)]
pub struct SymbolTable(HashMap<String, Declaration>);

impl SymbolTable {
    fn add_symbol(&mut self, name: String, decl: Declaration) {
        self.0.insert(name, decl);
    }

    pub fn get_symbol(&self, name: &str) -> Option<&Declaration> {
        self.0.get(name)
    }

    pub fn is_internal_symbol(&self, name: &str) -> bool {
        match self.get_symbol(name) {
            Some(Declaration { defined, .. }) => *defined,
            None => false,
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Conflicting declarations for {ident}")]
    ConflictingDeclaration {
        ident: String,
        #[label("first declared here")]
        first: Span,
        #[label("later declared here")]
        second: Span,
    },

    #[error("Duplicate definitions for {ident}")]
    DuplicateDefinition {
        ident: String,
        #[label("first defined here")]
        first: Span,
        #[label("later defined here")]
        second: Span,
    },

    #[error("Expression cannot be called")]
    UncallableExpression(#[label] Span),

    #[error("Invalid type: expected {expected}, got {actual}")]
    InvalidType {
        #[label]
        span: Span,
        expected: Type,
        actual: Type,
    },

    #[error("Could not find the type for {ident}; this is probably a bug")]
    NotFound {
        ident: String,
        #[label("here")]
        span: Span,
    },
}

#[derive(Default)]
pub(super) struct TypeChecker {
    pub symbol_table: SymbolTable,
    errs: Vec<SemanticError>,
}

pub(super) fn type_check() -> TypeChecker {
    TypeChecker::default()
}

impl ToErrors for TypeChecker {
    fn to_errors(self) -> SemanticErrors {
        SemanticErrors(self.errs)
    }
}

impl VisitMut for TypeChecker {
    fn visit_mut_var_decl_pre(&mut self, decl: &mut crate::parser::VarDecl) {
        self.symbol_table.add_symbol(
            decl.name.0.clone(),
            Declaration {
                ty: Type::Int,
                defined: matches!(decl.init, crate::parser::Initializer::ExpressionInit(_)),
                span: decl.name.1,
            },
        );
    }

    fn visit_mut_function_decl_pre(&mut self, decl: &mut crate::parser::FunctionDecl) {
        let fun_type = Type::Function(decl.args.num_args());
        let has_body = matches!(decl.body, crate::parser::FunctionBody::Block(_));
        let mut already_defined = false;
        let name = decl.name.0.clone();

        if let Some(old_decl) = self.symbol_table.get_symbol(&name) {
            if old_decl.ty != fun_type {
                self.errs.push(
                    Error::ConflictingDeclaration {
                        ident: name,
                        first: old_decl.span,
                        second: decl.name.1,
                    }
                    .into(),
                );
                return;
            }
            already_defined = old_decl.defined;
            if already_defined && has_body {
                self.errs.push(
                    Error::DuplicateDefinition {
                        ident: name,
                        first: old_decl.span,
                        second: decl.name.1,
                    }
                    .into(),
                );
                return;
            }
        }

        self.symbol_table.add_symbol(
            name,
            Declaration {
                ty: fun_type,
                defined: already_defined || has_body,
                span: decl.name.1,
            },
        );

        if has_body {
            for arg in decl.args.iter() {
                let (name, span) = &arg.name;
                self.symbol_table.add_symbol(
                    name.clone(),
                    Declaration {
                        ty: Type::Int,
                        defined: true,
                        span: *span,
                    },
                );
            }
        }
    }

    fn visit_mut_exp_pre(&mut self, exp: &mut Exp) {
        match exp {
            Exp::FunctionCall {
                ident,
                args,
                span: _,
            } => {
                let ty = match self.symbol_table.get_symbol(&ident.0) {
                    Some(decl) => &decl.ty,
                    None => {
                        self.errs.push(
                            Error::NotFound {
                                ident: ident.0.clone(),
                                span: ident.1,
                            }
                            .into(),
                        );
                        return;
                    }
                };

                match ty {
                    Type::Function(n) => {
                        if *n != args.0.len() {
                            self.errs.push(
                                Error::InvalidType {
                                    span: ident.1,
                                    expected: Type::Function(args.0.len()),
                                    actual: ty.clone(),
                                }
                                .into(),
                            );
                        }
                    }
                    _ => {
                        self.errs.push(Error::UncallableExpression(ident.1).into());
                    }
                }
            }
            Exp::Var { ident, span } => {
                let ty = match self.symbol_table.get_symbol(ident) {
                    Some(decl) => &decl.ty,
                    None => {
                        self.errs.push(
                            Error::NotFound {
                                ident: ident.clone(),
                                span: *span,
                            }
                            .into(),
                        );
                        return;
                    }
                };

                match ty {
                    Type::Int => {}
                    otherwise => {
                        self.errs.push(
                            Error::InvalidType {
                                span: *span,
                                expected: Type::Int,
                                actual: otherwise.clone(),
                            }
                            .into(),
                        );
                    }
                }
            }
            _ => {}
        }
    }
}
