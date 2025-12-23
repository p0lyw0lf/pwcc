use functional::TryCoalesce;

use crate::parser::Program;
use crate::parser::visit_mut::VisitMutBuilder;
use crate::parser::visit_mut::VisitMutExt;
use crate::semantic::SemanticErrors;
use crate::semantic::storage_check::*;

use super::parse;

fn run_storage_check(mut p: Program) -> Result<(), SemanticErrors> {
    let mut visitor = VisitMutBuilder::visit_mut_for_init_pre(check_for_init)
        .chain(check_function_decl_storage());
    visitor.visit_mut_program(&mut p);
    visitor.try_coalesce()?;
    Ok(())
}

#[test]
fn block_scope_function_decl() {
    let tree = parse(
        r#"
        static int foo(void) {
            extern int bar(void);
            return bar();
        }
        "#,
    );
    let result = run_storage_check(tree);
    assert!(
        result.is_ok(),
        "should allow extern declarations in block scope, and static at file scope"
    );

    let tree = parse(
        r#"
        int foo(void) {
            static int bar(void);
            return bar();
        }
        "#,
    );
    let result = run_storage_check(tree);
    assert!(result.is_err(), "should not allow static in block scope",);
}

#[test]
fn for_init_decl() {
    let tree = parse(
        r#"
        int main(void) {
            for (int i = 0;;) ;
        }
        "#,
    );
    let result = run_storage_check(tree);
    assert!(result.is_ok(), "bare declaration is OK");

    let tree = parse(
        r#"
        int main(void) {
            for (extern int i = 0;;) ;
        }
        "#,
    );
    let result = run_storage_check(tree);
    assert!(result.is_err(), "extern declaration is not OK");

    let tree = parse(
        r#"
        int main(void) {
            for (static int i = 0;;) ;
        }
        "#,
    );
    let result = run_storage_check(tree);
    assert!(result.is_err(), "static declaration is not OK");
}
