use functional_macros::ast;

#[ast]
mod ast {
    #[derive(Debug, PartialEq)]
    pub struct Function {
        pub name: String,
        pub body: Vec<Statement>,
    }

    #[derive(Debug, PartialEq)]
    pub struct Exp(pub isize);

    #[derive(Debug, PartialEq)]
    pub enum Statement {
        Null,
        Declare { var: String, init: Option<Exp> },
        Return(Exp),
    }
}

use ast::*;

#[test]
fn base_traits() {
    let x = Function {
        name: "main".into(),
        body: [Statement::Return(Exp(42))].into(),
    };
    assert_eq!(
        x,
        Function {
            name: "main".into(),
            body: [Statement::Return(Exp(42))].into(),
        }
    );
}
