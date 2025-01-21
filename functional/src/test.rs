use crate as functional;
use functional_macros::ast;

#[ast]
mod ast {
    use super::*;

    #[derive(Debug, PartialEq)]
    pub struct Function<'a, T> {
        pub name: &'a str,
        pub body: Vec<Statement<T, usize>>,
        pub output: Option<Exp>,
    }

    #[derive(Debug, PartialEq)]
    pub enum Statement<A, B> {
        Null,
        Declare {
            var: String,
            init: Option<A>,
            decl: Option<B>,
        },
        Return(Exp),
    }

    #[derive(Debug, PartialEq)]
    pub enum Exp {
        Lit(isize),
        Add { lhs: Box<Exp>, rhs: Box<Exp> },
    }
}
use ast::*;

use crate::Functor;
use crate::TryFunctor;

#[test]
fn functor_uniplate() {
    let x = Exp::Add {
        lhs: Box::new(Exp::Lit(1)),
        rhs: Box::new(Exp::Lit(2)),
    };

    let x_expected = Exp::Add {
        lhs: Box::new(Exp::Lit(3)),
        rhs: Box::new(Exp::Lit(4)),
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

#[test]
fn functor_biplate() {
    let x = Function {
        name: "main".into(),
        body: [Statement::<(), usize>::Return(Exp::Lit(42))].into(),
        output: None,
    };
    let x_expected = Function {
        name: "main".into(),
        body: [Statement::Return(Exp::Lit(69))].into(),
        output: None,
    };
    let x_actual = Functor::<Exp>::fmap(x, &mut |e| match e {
        Exp::Lit(42) => Exp::Lit(69),
        other => other,
    });
    assert_eq!(x_expected, x_actual);
}

#[test]
fn try_functor_uniplate() {
    let x = Exp::Add {
        lhs: Box::new(Exp::Lit(1)),
        rhs: Box::new(Exp::Lit(2)),
    };

    let x_expected = Exp::Add {
        lhs: Box::new(Exp::Lit(3)),
        rhs: Box::new(Exp::Lit(4)),
    };

    let mut transform = |e: Exp| -> Result<_, ()> {
        match e {
            Exp::Lit(i) => Ok(Exp::Lit(i + 2)),
            otherwise => Ok(otherwise),
        }
    };

    let x_actual = x.try_fmap(&mut transform);
    assert_eq!(x_expected, x_actual.expect("should not error"));

    let y = Exp::Lit(1);
    let y_expected = Exp::Lit(3);
    let y_actual = y.try_fmap(&mut transform);
    assert_eq!(y_expected, y_actual.expect("should not error"));
}

#[test]
fn try_functor_biplate() {
    let x = Function {
        name: "main".into(),
        body: [Statement::<(), usize>::Return(Exp::Lit(42))].into(),
        output: None,
    };
    let x_expected = Function {
        name: "main".into(),
        body: [Statement::Return(Exp::Lit(69))].into(),
        output: None,
    };
    let x_actual = TryFunctor::<Exp>::try_fmap(x, &mut |e| -> Result<_, ()> {
        match e {
            Exp::Lit(42) => Ok(Exp::Lit(69)),
            other => Ok(other),
        }
    });
    assert_eq!(x_expected, x_actual.expect("should not error"));
}
