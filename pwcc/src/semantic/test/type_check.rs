use crate::parser::Program;
use crate::parser::visit_mut::VisitMutExt;
use crate::semantic::SemanticErrors;
use crate::semantic::type_check::*;

use super::parse;

fn run_type_check(mut p: Program) -> Result<SymbolTable, SemanticErrors> {
    let mut visitor = type_check();
    visitor.visit_mut_program(&mut p);
    visitor.into()
}

#[test]
fn resolve_argument_decls() {
    let tree = parse(
        r#"
        int foo(int a, int b) {
            return a + b;
        }
        "#,
    );
    let result = run_type_check(tree);
    assert!(
        result.is_ok(),
        "expected to check successfully, got {}",
        unsafe { result.unwrap_err_unchecked() }
    );
}
