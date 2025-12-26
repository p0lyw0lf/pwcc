use crate::parser::BlockItem;
use crate::parser::Declaration;
use crate::parser::FunctionBody;
use crate::parser::Program;
use crate::parser::Statement;
use crate::parser::visit_mut::VisitMutExt;
use crate::semantic::SemanticErrors;
use crate::semantic::loop_labeling::*;

use super::parse;

fn run_labeling(function: &str, mut tree: Program) -> Result<Program, SemanticErrors> {
    let mut visitor = labeling(function);
    visitor.visit_mut_program(&mut tree);
    visitor.into().map(|_| tree)
}

#[test]
fn labels_breaks_in_while_loop() {
    let tree = parse(
        r#"
        int main(void) {
            int a = 10;
            while ((a = 1))
                break;
            return a;
        }
        "#,
    );
    let result = run_labeling("main", tree);
    assert!(
        result.is_ok(),
        "expected to succeed, got error {}",
        result.unwrap_err()
    );
    let mut tree = result.unwrap();
    let main_decl = tree.declarations.pop().expect("no main function");
    let main = match main_decl {
        Declaration::Function(f) => f,
        Declaration::Var(_) => panic!("expected function, got variable"),
    };
    let mut block = match main.body {
        FunctionBody::Defined(block) => block,
        FunctionBody::Declared(_) => panic!("expected definition, got declaration"),
    };
    let _ = block.items.pop().expect("no return statement");
    let while_stmt = match block.items.pop().expect("no while statement") {
        BlockItem::Statement(stmt) => match *stmt {
            Statement::WhileStmt(while_stmt) => while_stmt,
            otherwise => panic!("expected Statement::WhileStmt, got {otherwise:?}"),
        },
        otherwise => panic!("expected BlockItem::Statement, got {otherwise:?}"),
    };
    let while_label = while_stmt.label;
    let break_stmt = match while_stmt.body {
        Statement::BreakStmt(break_stmt) => break_stmt,
        otherwise => panic!("expected BreakStmt, got {otherwise:?}"),
    };
    assert!(
        break_stmt.label.is_some(),
        "break statement did not have label"
    );
    assert_eq!(while_label, break_stmt.label);
}
