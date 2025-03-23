use crate as functional;
use functional_macros::ast;

#[ast(typeclasses = [Functor])]
mod coherence_direct {
    #![allow(dead_code)]
    use super::*;

    #[include()]
    struct B<T>(T);

    struct AFiltered1<T, U> {
        bt: B<T>,
        bu: B<U>,
    }
    struct AFiltered2<T> {
        bt: B<T>,
        bi: B<i32>,
    }

    #[include()]
    struct C<T, U>(T, U);

    struct DFiltered1<T> {
        ct: C<T, i32>,
        cu: C<i32, T>,
    }
    struct DFiltered2<T, U> {
        ct: C<T, i32>,
        cu: C<i32, U>,
    }
}

#[ast(typeclasses = [Functor])]
mod variants {
    use super::*;
    #[derive(Debug, PartialEq)]
    struct Container {
        byte: Inner<i8>,
        word: Inner<i16>,
    }
    #[derive(Debug, PartialEq)]
    #[include()]
    struct Inner<T>(T);

    #[test]
    fn maps_variants() {
        let x = variants::Container {
            byte: Inner(1),
            word: Inner(3),
        };
        assert_eq!(
            Container {
                byte: Inner(2),
                word: Inner(3),
            },
            x.fmap(|mut i: Inner<i8>| {
                i.0 *= 2;
                i
            })
        );

        let x = Container {
            byte: Inner(1),
            word: Inner(3),
        };
        assert_eq!(
            Container {
                byte: Inner(1),
                word: Inner(9),
            },
            x.fmap(|mut i: Inner<i16>| {
                i.0 *= 3;
                i
            })
        );
    }
}

/// There should be a comment in the output when you test:
/// // Node A: filtering B due to C not being able to be transformed by it
#[ast(typeclasses = [Functor])]
mod coherence_indirect {
    use super::*;

    #[derive(Debug, PartialEq)]
    struct A<T> {
        b: B<T>,
        c: C<T>,
    }
    #[derive(Debug, PartialEq)]
    #[include()]
    struct B<T> {
        c: C<T>,
    }
    #[derive(Debug, PartialEq)]
    #[include()]
    struct C<T>(T);

    /// There shouldn't be a TInput -> TOutput implementation, but there should still be a T -> T
    /// implementation over B
    #[test]
    fn test_b() {
        let x = A {
            b: B { c: C(6i32) },
            c: C(9),
        };
        let x_expected = A {
            b: B { c: C(12i32) },
            c: C(9),
        };
        let x_actual = x.fmap(|mut b: B<i32>| {
            b.c.0 *= 2;
            b
        });
        assert_eq!(x_expected, x_actual);
    }

    /// There should still be a TInput -> TOutput implementation over C
    #[test]
    fn test_c() {
        let x = A {
            b: B { c: C(6i32) },
            c: C(9),
        };
        let x_expected = A {
            b: B {
                c: C("6".to_string()),
            },
            c: C("9".to_string()),
        };
        let x_actual = x.fmap(|C(i)| C(format!("{i}")));
        assert_eq!(x_expected, x_actual);
    }
}

#[ast(typeclasses = [Functor], extra_nodes = [i32])]
mod extra_nodes {
    use super::*;

    #[derive(Debug, PartialEq)]
    struct IntList {
        val: i32,
        next: Option<Box<IntList>>,
    }

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
        let x_actual = x.fmap(|i: i32| i * 2);
        assert_eq!(x_expected, x_actual);
    }
    // We don't support raw generics
    /*
    struct List<T> {
        val: T,
        next: Box<List<T>>,
    }
    */
}

#[ast(typeclasses = [Functor, TryFunctor])]
mod ast {
    use super::*;

    #[derive(Debug, PartialEq)]
    pub struct Function<'a, T> {
        pub name: &'a str,
        pub body: Vec<Statement<T, usize>>,
        pub output: Option<Exp>,
    }

    #[allow(dead_code)]
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
    #[include()]
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

        let transform = |e: Exp| match e {
            Exp::Lit(i) => Exp::Lit(i + 2),
            otherwise => otherwise,
        };

        let x_actual = x.fmap(transform);
        assert_eq!(x_expected, x_actual);

        let y = Exp::Lit(1);
        let y_expected = Exp::Lit(3);
        let y_actual = y.fmap(transform);
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
