use functional_macros::ast;

#[ast]
mod ast {
    use crate::Functor;

    #[derive(Debug, PartialEq)]
    pub struct Function<'a, T> {
        pub name: &'a str,
        pub body: Vec<Statement<T, usize>>,
    }

    #[derive(Debug, PartialEq)]
    pub enum Statement<A, B> {
        Null,
        Declare { var: String, init: Option<A>, decl: Option<B> },
        Return(Exp),
    }

    #[derive(Debug, PartialEq)]
    pub enum Exp {
        Lit(isize),
        Add { lhs: Box<Exp>, rhs: Box<Exp> },
    }

}

#[ast]
mod example {
    use crate::Functor;

    pub struct A<T> {
        b: B<T>,
        c: C<T>,
    }
    pub struct B<T> {
        c: C<T>,
    }
    pub struct C<T>(T);
}

use ast::*;

use crate::Functor;

#[test]
fn base_traits() {
    let x = Function {
        name: "main",
        body: [Statement::<(), usize>::Return(Exp::Lit(42))].into(),
    };
    assert_eq!(
        x,
        Function {
            name: "main",
            body: [Statement::Return(Exp::Lit(42))].into(),
        }
    );
}

/*
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

#[test]
fn biplate() {
    let x = Function {
        name: "main".into(),
        body: [Statement::Return(Exp::Lit(42))].into(),
    };
    let x_expected = Function {
        name: "main".into(),
        body: [Statement::Return(Exp::Lit(69))].into(),
    };
    let x_actual = Functor::<Exp>::fmap(x, &mut |e| match e {
        Exp::Lit(42) => Exp::Lit(69),
        other => other,
    });
    assert_eq!(x_expected, x_actual);
}
*/
