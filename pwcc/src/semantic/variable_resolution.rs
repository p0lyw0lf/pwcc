use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use functional::TryFunctor;

use crate::parser::Block;
use crate::parser::BlockItem;
use crate::parser::Declaration;
use crate::parser::DoWhileStmt;
use crate::parser::Exp;
use crate::parser::ExpressionStmt;
use crate::parser::ForInit;
use crate::parser::ForInitExp;
use crate::parser::ForStmt;
use crate::parser::Function;
use crate::parser::IfStmt;
use crate::parser::ReturnStmt;
use crate::parser::Statement;
use crate::parser::WhileStmt;
use crate::semantic::SemanticErrors;
use crate::semantic::UniqueLabelFactory;
use crate::span::SourceSpan;
use crate::span::Span;
use crate::span::Spanned;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Duplicate declaration: {second}")]
    DuplicateDeclaration {
        #[label("first declared here")]
        first: SourceSpan,
        #[label("later declared here")]
        second: Span<String>,
    },

    #[error("Unresolved variable: {0}")]
    UnresolvedVariable(#[label("in this expression")] Span<String>),

    #[error("Cannot assign to expression")]
    InvalidAssignment(#[label] SourceSpan),
}

/// Maps potentially-conflicting names to globally-unique names, in a given context
#[derive(Default)]
struct VariableMap {
    // All the mappings created in this block
    this_block: HashMap<String, Span<String>>,
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
    fn declared_in_block(&self, ident: &str) -> Option<SourceSpan> {
        self.this_block.get(ident).map(|d| d.span)
    }

    /// Inserts a new mapping into the current block
    fn new_mapping(&mut self, uf: &mut UniqueLabelFactory, ident: Span<String>) -> Span<String> {
        let unique_name = uf.unique_label(&ident).span(ident.span);
        self.this_block.insert(ident.inner, unique_name.clone());
        unique_name
    }

    /// Resolves a variable
    fn resolve(&mut self, ident: &str) -> Option<String> {
        if let Some(name) = self.this_block.get(ident) {
            return Some(name.inner.clone());
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
}

/// Very sadly, we _do_ have to implement recursive descent manually here, since the processing
/// wants to go out-to-in, on multiple mutually dependent node types.
/// We can handle out-to-in on a single node type, but not like this. Maybe something to consider
/// in the future! (TODO: automatic visitor generation?)
impl VariableResolution {
    fn resolve_decl(self: &mut Self, decl: Declaration) -> Result<Declaration, SemanticErrors> {
        if let Some(existing) = self.variable_map.declared_in_block(&decl.name) {
            return Err(Error::DuplicateDeclaration {
                first: existing,
                second: decl.name,
            })?;
        }

        Ok(Declaration {
            name: self.variable_map.new_mapping(&mut self.factory, decl.name),
            init: decl.init,
        })
    }

    /// TODO: this is broken until I get external nodes to work in fmap & co. Great!!!
    fn resolve_exp(self: &mut Self, exp: Span<Exp>) -> Result<Span<Exp>, SemanticErrors> {
        let span = exp.span;
        Ok(exp
            .inner
            .try_fmap(&mut |exp| -> Result<_, SemanticErrors> {
                match exp {
                    // Make sure all assignments are to Var, and nothing else
                    Exp::Assignment { lhs, op, rhs } => match *lhs.inner {
                        Exp::Var { .. } => Ok(Exp::Assignment { lhs, op, rhs }),
                        _ => Err(Error::InvalidAssignment(lhs.span))?,
                    },
                    // Replace all instantiations of Var with their unique name as determined by the
                    // declaration
                    Exp::Var { ident } => match self.variable_map.resolve(&ident) {
                        None => Err(Error::UnresolvedVariable(ident.span(span)))?,
                        Some(ident) => Ok(Exp::Var { ident }),
                    },
                    otherwise => Ok(otherwise),
                }
            })?
            .span(span))
    }

    fn resolve_stmt(self: &mut Self, stmt: Statement) -> Result<Statement, SemanticErrors> {
        // This function is pretty tricky! Need to control the recursion pretty tightly so that we
        // don't parse more expressions than we want to. We should only parse expressions when the
        // block is correct, which also depends on our depth into resolve_stmt.
        macro_rules! s {
            ($v:ident) => {
                $v.try_fmap_impl(
                    &mut |stmt| self.resolve_stmt(stmt),
                    functional::RecursiveCall::None,
                )?
            };
        }

        macro_rules! e {
            ($v: ident) => {
                self.resolve_exp($v)?
            };
        }

        macro_rules! oe {
            ($v: ident) => {
                match $v.inner {
                    Some(v) => Some(
                        self.resolve_exp(Span {
                            inner: v,
                            span: $v.span,
                        })?
                        .inner,
                    ),
                    None => None,
                }
                .span($v.span)
            };
        }

        // very sad that i'm not auto-generating more of this... ;-;
        // oh well at least it's mostly just the one spot!
        Ok(match stmt {
            Statement::Block(block) => Statement::Block(self.resolve_block(block)?),
            Statement::IfStmt(IfStmt {
                guard,
                body,
                else_stmt,
            }) => Statement::IfStmt(IfStmt {
                guard: e!(guard),
                body: s!(body),
                else_stmt: s!(else_stmt),
            }),
            Statement::WhileStmt(WhileStmt { label, guard, body }) => {
                Statement::WhileStmt(WhileStmt {
                    label,
                    guard: e!(guard),
                    body: s!(body),
                })
            }
            Statement::DoWhileStmt(DoWhileStmt { body, label, guard }) => {
                Statement::DoWhileStmt(DoWhileStmt {
                    body: s!(body),
                    label,
                    guard: e!(guard),
                })
            }
            // So ugly 😭
            Statement::ForStmt(ForStmt {
                label,
                init,
                exp1,
                exp2,
                body,
            }) => {
                self.variable_map.enter_block();
                let out = (|| -> Result<_, SemanticErrors> {
                    let init = match init.inner {
                        ForInit::ForInitExp(ForInitExp { exp }) => {
                            ForInit::ForInitExp(ForInitExp { exp: oe!(exp) })
                        }
                        ForInit::Declaration(decl) => {
                            ForInit::Declaration(self.resolve_decl(decl)?)
                        }
                    }
                    .span(init.span);
                    Ok(Statement::ForStmt(ForStmt {
                        label,
                        init,
                        exp1: {
                            println!("resolving {exp1:?}");
                            oe!(exp1)
                        },
                        exp2: {
                            println!("resolving {exp2:?}");
                            oe!(exp2)
                        },
                        body: s!(body),
                    }))
                })();
                self.variable_map.exit_block();
                out?
            }
            Statement::ExpressionStmt(ExpressionStmt { exp }) => {
                Statement::ExpressionStmt(ExpressionStmt { exp: e!(exp) })
            }
            Statement::ReturnStmt(ReturnStmt { exp }) => {
                Statement::ReturnStmt(ReturnStmt { exp: e!(exp) })
            }
            otherwise @ (Statement::BreakStmt(_)
            | Statement::ContinueStmt(_)
            | Statement::LabelStmt(_)
            | Statement::GotoStmt(_)
            | Statement::NullStmt(_)) => otherwise,
        })
    }

    fn resolve_block(self: &mut Self, block: Block) -> Result<Block, SemanticErrors> {
        self.variable_map.enter_block();

        let block = block.try_fmap_impl(
            &mut |item: BlockItem| -> Result<_, SemanticErrors> {
                Ok(match item {
                    BlockItem::Statement(stmt) => BlockItem::Statement(self.resolve_stmt(stmt)?),
                    BlockItem::Declaration(decl) => {
                        BlockItem::Declaration(self.resolve_decl(decl)?)
                    }
                })
            },
            // This makes it so we only map over the BlockItems that are direct children of this block, and
            // we recurse into the block manually.
            functional::RecursiveCall::None,
        );

        self.variable_map.exit_block();
        block
    }
}

pub(super) fn resolve_variables(mut f: Function) -> Result<Function, SemanticErrors> {
    let mut ctx = VariableResolution::default();

    f.body.inner = ctx.resolve_block(f.body.inner)?;

    Ok(f)
}
