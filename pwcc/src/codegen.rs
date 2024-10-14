use core::fmt::Debug;
use std::fmt::Display;

use functional::foldable;
use functional::functor;
use functional::Foldable;
use functional::Functor;

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
foldable!(type Location<S: State>);
functor!(type Location<I: State> -> <O: State>);

#[derive(Debug)]
pub struct Program<S: State> {
    pub function: Function<S>,
}

foldable!(struct Program<S: State> for Location<S> | { function, });
functor!(struct Program<I: State> -> <O: State> where Instructions<I> |> Instructions<O> | { function, .., });
functor!(struct Program<I: State> -> <O: State> where Location<I> |> Location<O> | { function, .., });

#[derive(Debug)]
pub struct Function<S: State> {
    pub name: String,
    pub instructions: Instructions<S>,
}

foldable!(struct Function<S: State> for Location<S> | { instructions, });
functor!(struct Function<I: State> -> <O: State> where Instructions<I> |> Instructions<O> | {
    instructions,
    ..,
    name,
});
functor!(struct Function<I: State> -> <O: State> where Location<I> |> Location<O> | {
    instructions,
    ..,
    name,
});

/// We separate this out into a newtype struct so that we can map over it as well as individual
/// instructions.
#[derive(Debug)]
pub struct Instructions<S: State>(pub Vec<Instruction<S>>);

foldable!(struct Instructions<S: State> for Location<S> | (+0,));
functor!(type Instructions<I: State> -> <O: State>);
functor!(struct Instructions<I: State> -> <O: State> where Location<I> |> Location<O> | (+0,));

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
    Cdq,
    AllocateStack {
        amount: usize,
    },
    Ret,
}

foldable!(enum Instruction<S: State> for Location<S> | {
    Mov { src, dst, },
    Unary { dst, },
    Binary { src, dst, },
    Idiv { denom, },
});
functor!(enum Instruction<I: State> -> <O: State> where Location<I> |> Location<O> | {
    Mov { src, dst, .., },
    Unary { dst, .., op, },
    Binary { src, dst, .., op, },
    Idiv { denom, .., },
    Cdq,
    AllocateStack { .., amount, },
    Ret,
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
    Imm(isize),
    Location(Location<S>),
}

foldable!(enum Operand<S: State> for Location<S> | {
    Location (+loc,),
});
functor!(enum Operand<I: State> -> <O: State> where Location<I> |> Location<O> | {
    Imm(-val,),
    Location(+loc,),
});

// Utilities for working with the Location newtype

impl<S: State> Location<S> {
    pub fn inner(self) -> S::Location {
        self.0
    }
    pub fn into<L, O: State<Location = L>>(self) -> Location<O>
    where
        S::Location: Into<L>,
    {
        Location(self.0.into())
    }
}
impl<S: State> AsRef<S::Location> for Location<S> {
    fn as_ref(&self) -> &S::Location {
        &self.0
    }
}
impl<S: State> AsMut<S::Location> for Location<S> {
    fn as_mut(&mut self) -> &mut S::Location {
        &mut self.0
    }
}
fn wrap<S: State>(loc: S::Location) -> Location<S> {
    Location(loc)
}

pub trait Identity<B> {
    type Mapped;
    fn identity(self) -> Self::Mapped;
}
impl<T, L, I: State<Location = L>, O: State<Location = L>> Identity<Location<O>> for T
where
    T: Functor<Location<O>, Input = Location<I>, Output = Location<O>>,
{
    type Mapped = T::Mapped;
    fn identity(self) -> Self::Mapped {
        self.fmap(&mut |x| wrap(x.inner()))
    }
}

impl<S: State> Display for Location<S>
where
    S::Location: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<S: State> Clone for Location<S>
where
    S::Location: Clone,
{
    fn clone(&self) -> Self {
        wrap(self.0.clone())
    }
}
