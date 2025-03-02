use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use functional::TryFunctor;

use crate::parser::Block;
use crate::parser::BlockItem;
use crate::parser::Declaration;
use crate::parser::Exp;
use crate::parser::Function;
use crate::parser::IfStmt;
use crate::parser::Statement;
use crate::semantic::SemanticErrors;
use crate::semantic::UniqueLabelFactory;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Duplicate declaration: {0}")]
    DuplicateDeclaration(String),
    #[error("Unresolved variable: {0}")]
    UnresolvedVariable(String),
    #[error("Cannot assign to: {0:?}")]
    InvalidAssignment(Exp),
    #[error("While analyzing {node}")]
    Context {
        node: String,
        #[source]
        #[diagnostic_source]
        err: Box<SemanticErrors>,
    },
}

fn context(err: SemanticErrors, node: &str) -> SemanticErrors {
    Error::Context {
        node: node.to_string(),
        err: Box::new(err),
    }
    .into()
}

/// Maps potentially-conflicting names to globally-unique names, in a given context
#[derive(Default)]
struct VariableMap {
    // All the mappings created in this block
    this_block: HashMap<String, String>,
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

    /// Returns whether the given ident is already declared in the current block
    fn declared_in_block(&self, ident: &str) -> bool {
        self.this_block.contains_key(ident)
    }

    /// Inserts a new mapping into the current block
    fn new_mapping(&mut self, uf: &mut UniqueLabelFactory, ident: String) -> String {
        let unique_name = uf.unique_label(&ident);
        self.this_block.insert(ident, unique_name.clone());
        unique_name
    }

    /// Resolves a variable
    fn resolve(&mut self, ident: &str) -> Option<String> {
        if let Some(name) = self.this_block.get(ident) {
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
        let name = decl.name;
        if self.variable_map.declared_in_block(&name) {
            return Err(Error::DuplicateDeclaration(name))?;
        }

        Ok(Declaration {
            name: self.variable_map.new_mapping(&mut self.factory, name),
            init: decl.init,
        })
    }

    fn resolve_exp(self: &mut Self, exp: Exp) -> Result<Exp, SemanticErrors> {
        match exp {
            Exp::Assignment { lhs, op, rhs } => match *lhs {
                Exp::Var { .. } => Ok(Exp::Assignment { lhs, op, rhs }),
                otherwise => Err(Error::InvalidAssignment(otherwise))?,
            },
            Exp::Var { ident } => match self.variable_map.resolve(&ident) {
                None => Err(Error::UnresolvedVariable(ident))?,
                Some(ident) => Ok(Exp::Var { ident }),
            },
            otherwise => Ok(otherwise),
        }
    }

    fn resolve_stmt(self: &mut Self, stmt: Statement) -> Result<Statement, SemanticErrors> {
        // This is by far the saddest function...

        macro_rules! s {
            ($v:ident, $context:literal) => {
                $v.try_fmap_impl(
                    &mut |stmt| self.resolve_stmt(stmt),
                    functional::RecursiveCall::None,
                )
                .map_err(|err| context(err, $context))?
            };
        }

        macro_rules! e {
            ($v:ident, $context:literal) => {
                $v.try_fmap(&mut |exp| self.resolve_exp(exp))
                    .map_err(|err| context(err, $context))?
            };
        }

        Ok(match stmt {
            Statement::Block(block) => Statement::Block(self.resolve_block(block)?),
            Statement::IfStmt(IfStmt {
                exp,
                body,
                else_stmt,
            }) => Statement::IfStmt(IfStmt {
                exp: e!(exp, "if statement condition"),
                body: s!(body, "if statement body"),
                else_stmt: s!(else_stmt, "else statement body"),
            }),
            otherwise @ Statement::ExpressionStmt(_) => {
                e!(otherwise, "expression statement")
            }
            otherwise @ Statement::ReturnStmt(_) => {
                e!(otherwise, "return statement")
            }
            otherwise @ (Statement::LabelStmt(_)
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

    f.body = ctx.resolve_block(f.body)?;

    Ok(f)
}
