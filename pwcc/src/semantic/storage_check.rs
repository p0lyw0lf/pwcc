use miette::Diagnostic;
use thiserror::Error;

use pwcc_util::span::Span;
use pwcc_util::span::Spanned;

use crate::parser::ForInit;
use crate::parser::StorageClass;
use crate::parser::visit_mut::VisitMut;
use crate::semantic::SemanticError;
use crate::semantic::SemanticErrors;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error("Cannot specify storage in for loop initializer")]
    ForInitStorage(#[label] Span),

    #[error("Block-scope function declaration for {ident} cannot be static")]
    BlockScopeStatic {
        ident: String,
        #[label]
        span: Span,
    },
}

/// Checks to make sure that all for loop initializers have _no_ storage.
pub(super) fn check_for_init(for_init: &mut ForInit) -> Result<(), SemanticErrors> {
    match for_init {
        ForInit::Decl(decl) => {
            if decl.ty.storage.is_some() {
                Err(Error::ForInitStorage(decl.span()).into())
            } else {
                Ok(())
            }
        }
        ForInit::Exp(_) => {
            // Expressions include no storage, and are always fine
            Ok(())
        }
    }
}

/// Checks to make sure that all functions declared outside of file scope have non-static storage.
pub(super) fn check_function_decl_storage() -> impl VisitMut + Into<Result<(), SemanticErrors>> {
    #[derive(Default)]
    struct FunctionStorageChecker {
        errs: Vec<SemanticError>,
        nesting_level: usize,
    }

    impl VisitMut for FunctionStorageChecker {
        fn visit_mut_block_pre(&mut self, _block: &mut crate::parser::Block) {
            self.nesting_level += 1;
        }

        fn visit_mut_block_post(&mut self, _block: &mut crate::parser::Block) {
            self.nesting_level -= 1;
        }

        fn visit_mut_function_decl_pre(&mut self, decl: &mut crate::parser::FunctionDecl) {
            if self.nesting_level == 0 {
                return;
            }

            // TODO: there are some utilities in the `type_check` module that I'd probably like to
            // make into a trait, if this pattern comes up too much more often.
            if matches!(decl.ty.storage, Some(StorageClass::Static(_))) {
                self.errs.push(
                    Error::BlockScopeStatic {
                        ident: decl.name.0.clone(),
                        span: decl.name.1,
                    }
                    .into(),
                );
            }
        }
    }

    impl From<FunctionStorageChecker> for Result<(), SemanticErrors> {
        fn from(v: FunctionStorageChecker) -> Self {
            if v.errs.is_empty() {
                Ok(())
            } else {
                Err(SemanticErrors(v.errs))
            }
        }
    }

    FunctionStorageChecker::default()
}
