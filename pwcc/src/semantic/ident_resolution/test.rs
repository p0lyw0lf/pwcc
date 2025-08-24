use functional::TryFunctor;

use super::*;

use crate::lexer::lex;
use crate::parser::FromTokens;
use crate::parser::Program;

fn parse(source: &str) -> Program {
    Program::from_tokens(&mut lex(source).expect("lex failed").into_iter()).expect("parse failed")
}

#[test]
fn resolves_global_function_call() {
    let tree = parse(
        r#"
        int foo();
        int main(void) {
            foo();
        }
    "#,
    );
    let result = tree.try_fmap(resolve_idents);
    assert!(
        result.is_ok(),
        "expected to succeed, got error {}",
        result.unwrap_err()
    );
}

#[test]
fn resolves_local_function_call() {
    let tree = parse(
        r#"
        int main(void) {
            int foo();
            foo();
        }
        "#,
    );
    let result = tree.try_fmap(resolve_idents);
    assert!(
        result.is_ok(),
        "expected to succeed, got error {}",
        result.unwrap_err()
    );
}

#[test]
fn no_shadow_decl_arg_list() {
    let tree = parse(
        r#"
        int foo(int a, int a);
        "#,
    );
    let result = tree.try_fmap(resolve_idents);
    assert!(result.is_err());
}

#[test]
fn no_shadow_decl_arg_body() {
    let tree = parse(
        r#"
        int foo(int a) {
            int a = 3;
            return a;
        }
        "#,
    );
    let result = tree.try_fmap(resolve_idents);
    assert!(result.is_err());
}
