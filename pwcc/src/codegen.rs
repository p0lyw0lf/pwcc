use core::fmt::Debug;
use core::convert::From;

use functional::foldable;
use functional::functor;

pub mod hardware;
pub mod pseudo;
pub mod stack;

/// A State represents an assembly AST, where the operands of certain functions are abstracted.
pub trait State: Debug + Sized {
    type Location: Debug + Sized + foldable::Foldable<Self::Location>;
}

/// Newtype needed to avoid "unconstrained type" errors
#[derive(Debug)]
pub struct Location<S: State> { inner: S::Location }
foldable!(struct<S: State> Location for S::Location { inner, });
functor!(type<I: State, O: State> Location);
impl<S: State> From<S::Location> for Location<S> {
    fn from(value: S::Location) -> Self {
        Self { inner: value }
    }
}

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
pub struct Instructions<S: State> {
    pub instructions: Vec<Instruction<S>>,
}

foldable!(struct<S: State> Instructions for Location<S> { instructions, });
functor!(type<I: State, O: State> Instructions);
functor!(struct<I: State, O: State> Instructions for Location<I> |> Location<O> { instructions, .. });

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
    Location { location, .. },
});
