use crate as functional;
use functional_macros::ast;

#[ast(typeclasses = [Functor])]
mod variants {
    use super::*;
    #[derive(Debug, PartialEq)]
    struct Container {
        byte: Inner<i8>,
        word: Inner<i16>,
    }
    #[derive(Debug, PartialEq)]
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
            x.fmap(&mut |mut i: Inner<i8>| {
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

struct Wrap<T>(T);
impl<T, Output> crate::Functor<Output> for Wrap<T>
where
    T: crate::Functor<Output>,
{
    type Input = T::Input;
    type Mapped = Wrap<T::Mapped>;
    fn fmap_impl(
        self,
        f: &mut impl FnMut(Self::Input) -> Output,
        how: crate::RecursiveCall,
    ) -> Self::Mapped {
        Wrap(self.0.fmap_impl(f, how))
    }
}

#[ast(extra_nodes = [Wrap<T>], typeclasses = [Functor])]
mod extra_nodes {
    use super::*;

    struct IntList {
        val: i32,
        next: Wrap<Box<IntList>>,
    }

    struct List<T> {
        val: T,
        next: Box<List<T>>,
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
