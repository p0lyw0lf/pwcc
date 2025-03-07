use crate as functional;
use crate::functor_base;
use crate::functor_wrap;
use functional_macros::ast;

#[derive(Debug, PartialEq)]
struct Wrap<T>(T);
functor_wrap!(Wrap);

#[ast(typeclasses = [Functor])]
mod variants {
    use super::*;
    #[derive(Debug, PartialEq)]
    struct Container {
        byte: Inner<i8>,
        word: Inner<i16>,
        long: Wrap<i32>,
    }

    #[derive(Debug, PartialEq)]
    struct Inner<T>(T);

    #[test]
    fn maps_variants() {
        let x = variants::Container {
            byte: Inner(1),
            word: Inner(3),
            long: Wrap(9),
        };
        assert_eq!(
            Container {
                byte: Inner(2),
                word: Inner(3),
                long: Wrap(9),
            },
            x.fmap(&mut |mut i: Inner<i8>| {
                i.0 *= 2;
                i
            })
        );

        let x = Container {
            byte: Inner(1),
            word: Inner(3),
            long: Wrap(10),
        };
        assert_eq!(
            Container {
                byte: Inner(1),
                word: Inner(9),
                long: Wrap(10),
            },
            x.fmap(&mut |mut i: Inner<i16>| {
                i.0 *= 3;
                i
            })
        );
    }
}

/// There should be a comment in the output when you test:
/// // Node A: filtering B due to C not being able to be transformed by it
#[ast(typeclasses = [Functor])]
mod coherence {
    use super::*;

    struct A<T> {
        b: B<T>,
        c: C<T>,
    }
    pub struct B<T> {
        c: C<T>,
    }
    pub struct C<T>(T);
}

#[ast(typeclasses = [Functor])]
mod raw_extern {
    use super::*;

    #[derive(Debug, PartialEq)]
    struct IntList {
        val: i32,
        next: Option<Box<i32>>,
    }

    /*
    #[test]
    fn test() {
        let x = IntList {
            val: 0,
            next: Some(Box::new(IntList {
                val: 1,
                next: Some(Box::new(IntList { val: 2, next: None })),
            })),
        };
        let x_expected = IntList {
            val: 0,
            next: Some(Box::new(IntList {
                val: 2,
                next: Some(Box::new(IntList { val: 4, next: None })),
            })),
        };
        let x_actual = x.fmap(&mut |i: i32| i * 2);
        assert_eq!(x_expected, x_actual);
    }
    */
}

#[ast(typeclasses = [Functor])]
mod raw_generic {
    use super::*;

    #[derive(Debug, PartialEq)]
    struct List<T> {
        val: T,
        next: Option<Box<List<T>>>,
    }

    #[test]
    fn test() {
        let x = List {
            val: 0i32,
            next: Some(Box::new(List {
                val: 1,
                next: Some(Box::new(List { val: 2, next: None })),
            })),
        };
        let x_expected = List {
            val: 1i32,
            next: Some(Box::new(List {
                val: 4,
                next: Some(Box::new(List { val: 9, next: None })),
            })),
        };
        // We don't support fmap-ing over raw generics
        let x_actual = x.fmap(&mut |List { val: i, next }| List {
            val: (i + 1) * (i + 1),
            next,
        });
        assert_eq!(x_expected, x_actual);
    }
}

/*
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
}
*/
