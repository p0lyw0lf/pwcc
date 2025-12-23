use std::collections::HashMap;
use std::fmt::Display;

use miette::Diagnostic;
use thiserror::Error;

use pwcc_util::span::Span;

use crate::parser::DeclArg;
use crate::parser::Exp;
use crate::parser::Initializer;
use crate::parser::StorageClass;
use crate::parser::visit_mut::VisitMut;
use crate::semantic::SemanticError;
use crate::semantic::SemanticErrors;

#[derive(Debug, Clone)]
pub enum Type {
    Var(VarAttr),
    Function(FunctionAttr),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(_) => write!(f, "int"),
            Type::Function(FunctionAttr {
                parameter_count: 0, ..
            }) => write!(f, "int function(void)"),
            Type::Function(FunctionAttr {
                parameter_count: n, ..
            }) => {
                write!(f, "int function(")?;
                for _ in 0..n - 1 {
                    write!(f, "int, ")?;
                }
                write!(f, "int)")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum VarAttr {
    Static {
        initial_value: VarInit,
        global: bool,
    },
    Local,
}

impl VarAttr {
    fn global(&self) -> bool {
        match self {
            Self::Static {
                initial_value: _,
                global,
            } => *global,
            Self::Local => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum VarInit {
    Tentative,
    Initial(isize),
    None,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionAttr {
    pub parameter_count: usize,
    pub defined: bool,
    pub global: bool,
}

fn is_static(storage: &Option<StorageClass>) -> bool {
    matches!(storage, Some(StorageClass::Static(_)))
}

fn is_extern(storage: &Option<StorageClass>) -> bool {
    matches!(storage, Some(StorageClass::Extern(_)))
}

pub struct Declaration {
    ty: Type,
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
            Some(Declaration {
                ty: Type::Function(attr),
                ..
            }) => attr.defined,
            // TODO: do we need to do this for variables too?
            Some(Declaration {
                ty: Type::Var(_), ..
            })
            | None => false,
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

    #[error("Static function declaration follows non-static for {ident}")]
    StaticFollowsNonStatic {
        ident: String,
        #[label("first declared here")]
        first: Span,
        #[label("later declared as static here")]
        second: Span,
    },

    #[error("File scope initializer must be constant for {ident}")]
    NonConstantInitializer {
        ident: String,
        #[label]
        span: Span,
        #[source]
        cause: crate::evaluator::Error,
    },

    #[error("Cannot have initializer on local extern variable declaration for {ident}")]
    LocalExternInitializer {
        ident: String,
        #[label]
        span: Span,
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
    symbol_table: SymbolTable,
    nesting_level: usize,
    errs: Vec<SemanticError>,
    function_has_body: bool,
}

pub(super) fn type_check() -> TypeChecker {
    TypeChecker::default()
}

impl From<TypeChecker> for Result<SymbolTable, SemanticErrors> {
    fn from(v: TypeChecker) -> Self {
        if v.errs.is_empty() {
            Ok(v.symbol_table)
        } else {
            Err(SemanticErrors(v.errs))
        }
    }
}

impl VisitMut for TypeChecker {
    fn visit_mut_block_pre(&mut self, _block: &mut crate::parser::Block) {
        self.nesting_level += 1;
    }

    fn visit_mut_block_post(&mut self, _block: &mut crate::parser::Block) {
        self.nesting_level -= 1;
    }

    fn visit_mut_var_decl_pre(&mut self, decl: &mut crate::parser::VarDecl) {
        let name = decl.name.0.clone();

        // Extracts a VarAttr from an existing declaration, or returns with an error.
        macro_rules! var_attrs {
            ($decl:expr) => {
                match &$decl.ty {
                    Type::Var(attrs) => attrs,
                    Type::Function(_) => {
                        self.errs.push(
                            Error::ConflictingDeclaration {
                                ident: name,
                                first: $decl.span,
                                second: decl.name.1,
                            }
                            .into(),
                        );
                        return;
                    }
                }
            };
        }

        // Evaluates an initializer, returning an error if it's non-constant.
        macro_rules! evaluate {
            ($exp:expr) => {
                match crate::evaluator::evaluate($exp) {
                    Ok(v) => v,
                    Err(err) => {
                        self.errs.push(
                            Error::NonConstantInitializer {
                                ident: name.clone(),
                                span: decl.name.1,
                                cause: err,
                            }
                            .into(),
                        );

                        // Continue on to catch other errors. Return a default value so we can.
                        0
                    }
                }
            };
        }

        if self.nesting_level == 0 {
            // at file scope
            let mut initial_value = match &decl.init {
                Initializer::Defined(exp, _) => VarInit::Initial(evaluate!(exp)),
                Initializer::Declared(_) => match decl.ty.storage {
                    Some(StorageClass::Extern(_)) => VarInit::None,
                    Some(StorageClass::Static(_)) | None => VarInit::Tentative,
                },
            };

            let mut global = !is_static(&decl.ty.storage);

            if let Some(old_decl) = self.symbol_table.get_symbol(&name) {
                let attrs = var_attrs!(old_decl);

                if is_extern(&decl.ty.storage) {
                    global = attrs.global();
                } else if attrs.global() != global {
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

                if let VarAttr::Static {
                    initial_value: old_initial_value,
                    global: _,
                } = &attrs
                {
                    match (old_initial_value, &decl.init) {
                        (VarInit::Initial(_), Initializer::Defined(_, _)) => {
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
                        (old_initial_value @ VarInit::Initial(_), Initializer::Declared(_)) => {
                            initial_value = old_initial_value.clone();
                        }
                        (VarInit::Tentative, Initializer::Defined(_, _)) => {}
                        (VarInit::Tentative, Initializer::Declared(_)) => {
                            initial_value = VarInit::Tentative;
                        }
                        (VarInit::None, _) => {}
                    }
                }
            }

            let attrs = VarAttr::Static {
                initial_value,
                global,
            };
            self.symbol_table.add_symbol(
                name,
                Declaration {
                    ty: Type::Var(attrs),
                    span: decl.name.1,
                },
            );
        } else {
            // not at file scope
            match &decl.ty.storage {
                Some(StorageClass::Extern(_)) => {
                    match &decl.init {
                        Initializer::Declared(_) => {}
                        Initializer::Defined(_, _) => {
                            self.errs.push(
                                Error::LocalExternInitializer {
                                    ident: name,
                                    span: decl.name.1,
                                }
                                .into(),
                            );
                            return;
                        }
                    }

                    if let Some(old_decl) = self.symbol_table.get_symbol(&name) {
                        let _ = var_attrs!(old_decl);
                        // Duplicate declaration. That is OK, keep the existing declaration.
                    } else {
                        self.symbol_table.add_symbol(
                            name,
                            Declaration {
                                ty: Type::Var(VarAttr::Static {
                                    initial_value: VarInit::None,
                                    global: true,
                                }),
                                span: decl.name.1,
                            },
                        );
                    }
                }
                Some(StorageClass::Static(_)) => {
                    let initial_value = match &decl.init {
                        Initializer::Defined(exp, _) => VarInit::Initial(evaluate!(exp)),
                        Initializer::Declared(_) => VarInit::Initial(0),
                    };
                    self.symbol_table.add_symbol(
                        name,
                        Declaration {
                            ty: Type::Var(VarAttr::Static {
                                initial_value,
                                global: false,
                            }),
                            span: decl.name.1,
                        },
                    );
                }
                None => {
                    self.symbol_table.add_symbol(
                        decl.name.0.clone(),
                        Declaration {
                            ty: Type::Var(VarAttr::Local),
                            span: decl.name.1,
                        },
                    );
                }
            }
        }
    }

    fn visit_mut_function_decl_pre(&mut self, decl: &mut crate::parser::FunctionDecl) {
        let has_body = matches!(decl.body, crate::parser::FunctionBody::Defined(_));
        let mut already_defined = false;
        let mut global = !is_static(&decl.ty.storage);
        let name = decl.name.0.clone();

        if let Some(old_decl) = self.symbol_table.get_symbol(&name) {
            let attrs = match &old_decl.ty {
                Type::Function(
                    attrs @ FunctionAttr {
                        parameter_count,
                        defined: _,
                        global: _,
                    },
                ) if *parameter_count == decl.args.num_args() => attrs,
                _ => {
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
            };
            already_defined = attrs.defined;
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

            if attrs.global && is_static(&decl.ty.storage) {
                self.errs.push(
                    Error::StaticFollowsNonStatic {
                        ident: name,
                        first: old_decl.span,
                        second: decl.name.1,
                    }
                    .into(),
                );
                return;
            }

            global = attrs.global;
        }

        self.symbol_table.add_symbol(
            name,
            Declaration {
                ty: Type::Function(FunctionAttr {
                    parameter_count: decl.args.num_args(),
                    defined: already_defined || has_body,
                    global,
                }),
                span: decl.name.1,
            },
        );

        self.function_has_body = has_body;
    }

    fn visit_mut_decl_arg_pre(&mut self, arg: &mut DeclArg) {
        if !self.function_has_body {
            return;
        }

        let (name, span) = &arg.name;
        self.symbol_table.add_symbol(
            name.clone(),
            Declaration {
                ty: Type::Var(VarAttr::Local),
                span: *span,
            },
        );
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
                    Type::Function(attrs) => {
                        if attrs.parameter_count != args.0.len() {
                            self.errs.push(
                                Error::InvalidType {
                                    span: ident.1,
                                    expected: Type::Function(FunctionAttr {
                                        parameter_count: args.0.len(),
                                        defined: false,
                                        global: false,
                                    }),
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
                    Type::Var(_) => {}
                    _ => {
                        self.errs.push(
                            Error::InvalidType {
                                span: *span,
                                expected: Type::Var(VarAttr::Local),
                                actual: ty.clone(),
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
