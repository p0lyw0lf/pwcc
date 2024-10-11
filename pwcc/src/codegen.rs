use core::fmt::Debug;

use functional::foldable;
use functional::foldable::Foldable;
use functional::functor;
use functional::functor::Functor;

pub mod hardware;
pub mod pseudo;
pub mod stack;

/// A State represents an assembly AST, where the operands of certain functions are abstracted.
pub trait State: Debug + Sized {
    type Location: Debug + Sized + foldable::Foldable<Self::Location>;
}

/// Newtype needed to avoid "unconstrained type" errors
#[derive(Debug)]
pub struct Location<S: State>(pub S::Location);
impl<S: State> Location<S> {
    pub fn inner(self) -> S::Location {
        self.0
    }
    pub fn wrap(inner: S::Location) -> Self {
        Self(inner)
    }
}
foldable!(type<S: State> Location);
functor!(type<I: State, O: State> Location);

#[derive(Debug)]
pub struct Program<S: State> {
    pub function: Function<S>,
}

foldable!(struct<S: State> Program for Location<S> { function, });
functor!(struct<I: State, O: State> Program for Instructions<I> |> Instructions<O> { function, .. });
functor!(struct<I: State, O: State> Program for Location<I> |> Location<O> { function, .. });

#[derive(Debug)]
pub struct Function<S: State> {
    pub name: String,
    pub instructions: Instructions<S>,
}

foldable!(struct<S: State> Function for Location<S> { instructions, });
functor!(struct<I: State, O: State> Function for Instructions<I> |> Instructions<O> { instructions, .. name, });
functor!(struct<I: State, O: State> Function for Location<I> |> Location<O> { instructions, .. name, });

/// We separate this out into a newtype struct so that we can map over it as well as individual
/// instructions.
#[derive(Debug)]
pub struct Instructions<S: State>(pub Vec<Instruction<S>>);

impl<I: State> Foldable<Location<I>> for Instructions<I> {
    fn foldl<'s, 'f: 's, B>(&'s self, f: fn(B, &'s Location<I>) -> B, mut acc: B) -> B {
        for i in self.0.iter() {
            acc = i.foldl(f, acc);
        }
        acc
    }
}
functor!(type<I: State, O: State> Instructions);
impl<I: State, O: State> Functor<Location<O>> for Instructions<I> {
    type Input = Location<I>;
    type Output = Location<O>;
    type Mapped = Instructions<O>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
        Instructions(
            self.0
                .into_iter()
                .map(|i| Functor::<Location<O>>::fmap(i, f))
                .collect(),
        )
    }
}

#[derive(Debug)]
pub enum Instruction<S: State> {
    Mov {
        src: Operand<S>,
        dst: Location<S>,
    },
    Unary {
        op: UnaryOp,
        dst: Location<S>,
    },
    Binary {
        op: BinaryOp,
        src: Operand<S>,
        dst: Location<S>,
    },
    Idiv {
        denom: Operand<S>,
    },
    Cdq {},
    AllocateStack {
        amount: usize,
    },
    Ret {},
}

foldable!(enum<S: State> Instruction for Location<S> {
    Mov { src, dst, },
    Unary { dst, },
    Binary { src, dst, },
    Idiv { denom, },
});
functor!(enum<I: State, O: State> Instruction for Location<I> |> Location<O> {
    Mov { src, dst, .. },
    Unary { dst, .. op, },
    Binary { src, dst, .. op, },
    Idiv { denom, .. },
    Cdq { .. },
    AllocateStack { .. amount, },
    Ret { .. },
});

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
}

#[derive(Debug)]
pub enum Operand<S: State> {
    Imm { val: isize },
    Location { location: Location<S> },
}

foldable!(enum<S: State> Operand for Location<S> {
    Location { location, },
});
functor!(enum<I: State, O: State> Operand for Location<I> |> Location<O> {
    Imm { .. val, },
    Location { location, .. },
});

pub trait Passthrough<B> {
    type Mapped;
    fn passthrough(self) -> Self::Mapped;
}

impl<T, I, O> Passthrough<Location<O>> for T
where
    I: State,
    O: State<Location=I::Location>,
    T: Functor<Location<O>, Input=Location<I>, Output=Location<O>>,
{
    type Mapped = T::Mapped;
    fn passthrough(self) -> Self::Mapped {
        self.fmap(&mut |x: Location<I>| Location::<O>(x.inner()))
    }
}
