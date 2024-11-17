use functional_macros::ast;

#[ast]
mod ast {
    use crate::Functor;

    #[derive(Debug, PartialEq)]
    pub struct Function {
        pub name: String,
        pub body: Vec<Statement>,
    }

    #[derive(Debug, PartialEq)]
    pub enum Exp {
        Lit(isize),
        Add { lhs: Box<Exp>, rhs: Box<Exp> },
    }

    #[derive(Debug, PartialEq)]
    pub enum Statement {
        Null,
        Declare { var: String, init: Option<Exp> },
        Return(Exp),
    }
}

use ast::*;

use crate::Functor;

#[test]
fn base_traits() {
    let x = Function {
        name: "main".into(),
        body: [Statement::Return(Exp::Lit(42))].into(),
    };
    assert_eq!(
        x,
        Function {
            name: "main".into(),
            body: [Statement::Return(Exp::Lit(42))].into(),
        }
    );
}

#[test]
fn uniplate() {
    let x = Exp::Add {
        lhs: Box::new(Exp::Lit(1)),
        rhs: Box::new(Exp::Lit(2)),
    };

    // Unfortunately, we don't go inside nodes just yet 😔
    let x_expected = Exp::Add {
        lhs: Box::new(Exp::Lit(1)),
        rhs: Box::new(Exp::Lit(2)),
    };

    let mut transform = |e: Exp| match e {
        Exp::Lit(i) => Exp::Lit(i + 2),
        otherwise => otherwise,
    };

    let x_actual = x.fmap(&mut transform);

    assert_eq!(x_expected, x_actual);

    let y = Exp::Lit(1);
    let y_expected = Exp::Lit(3);
    let y_actual = y.fmap(&mut transform);
    assert_eq!(y_expected, y_actual);
}
