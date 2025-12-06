use functional::TryFunctor;

use crate::lexer::lex;
use crate::parser::BlockItem;
use crate::parser::FromTokens;
use crate::parser::FunctionBody;
use crate::parser::Program;
use crate::parser::Statement;
use crate::semantic::loop_labeling::*;

fn parse(source: &str) -> Program {
    Program::from_tokens(&mut lex(source).expect("lex failed").into_iter()).expect("parse failed")
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
    let result = tree.try_fmap(labeling);
    assert!(
        result.is_ok(),
        "expected to succeed, got error {}",
        result.unwrap_err()
    );
    let mut tree = result.unwrap();
    let main = tree.functions.pop().expect("no main function");
    let mut block = match main.body {
        FunctionBody::Block(block) => block,
        FunctionBody::Semicolon(_) => panic!("expected Block, got Semicolon"),
    };
    let _ = block.items.pop().expect("no return statement");
    let while_stmt = match block.items.pop().expect("no while statement") {
        BlockItem::Statement(Statement::WhileStmt(while_stmt)) => while_stmt,
        otherwise => panic!("expected WhileStmt, got {otherwise:?}"),
    };
    let while_label = while_stmt.label;
    let break_stmt = match *while_stmt.body {
        Statement::BreakStmt(break_stmt) => break_stmt,
        otherwise => panic!("expected BreakStmt, got {otherwise:?}"),
    };
    assert!(
        break_stmt.label.is_some(),
        "break statement did not have label"
    );
    assert_eq!(while_label, break_stmt.label);
}
