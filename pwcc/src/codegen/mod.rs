use core::fmt::Debug;
use std::fmt::Display;

use functional::Foldable;
use functional::Functor;

use crate::tacky::Identifier;

pub mod hardware;
pub mod pseudo;
pub mod stack;

/// A State represents an assembly AST, where the operands of certain functions are abstracted.
pub trait State: Debug + Sized {
    #[cfg(test)]
    type Location: Debug + Sized + PartialEq;

    #[cfg(not(test))]
    type Location: Debug + Sized;
}

#[functional_macros::ast(typeclasses = [Functor, TryFunctor, Foldable])]
mod ast {
    use super::*;

    /// Newtype needed to avoid "unconstrained type" errors
    #[derive(Debug)]
    #[cfg_attr(test, derive(PartialEq))]
    #[include()]
    pub struct Location<S: State>(pub S::Location);

    #[derive(Debug)]
    pub struct Program<S: State> {
        pub functions: Vec<Function<S>>,
    }

    #[derive(Debug)]
    pub enum Declaration<S: State> {
        Function(Function<S>),
        StaticVariable(StaticVariable),
    }

    #[derive(Debug)]
    #[cfg_attr(test, derive(PartialEq))]
    pub struct Function<S: State> {
        pub name: Identifier,
        pub global: bool,
        pub instructions: Instructions<S>,
    }

    #[derive(Debug)]
    pub struct StaticVariable {
        pub name: Identifier,
        pub global: bool,
        pub initial_value: isize,
    }

    /// We separate this out into a newtype struct so that we can map over it as well as individual
    /// instructions.
    #[derive(Debug)]
    #[cfg_attr(test, derive(PartialEq))]
    #[include()]
    pub struct Instructions<S: State>(pub Vec<Instruction<S>>);

    #[derive(Debug)]
    #[cfg_attr(test, derive(PartialEq))]
    #[include()]
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
        Cmp {
            left: Location<S>,
            right: Operand<S>,
        },
        Idiv {
            denom: Operand<S>,
        },
        Cdq,
        Jmp(Identifier),
        JmpCC(CondCode, Identifier),
        SetCC(CondCode, Location<S>),
        Label(Identifier),
        AllocateStack {
            amount: usize,
        },
        DeallocateStack {
            amount: usize,
        },
        Push(Operand<S>),
        Call(Identifier),
        Ret,
    }

    #[derive(Debug, Copy, Clone)]
    #[cfg_attr(test, derive(PartialEq))]
    pub enum UnaryOp {
        Neg,
        Not,
    }

    #[derive(Debug, Copy, Clone)]
    #[cfg_attr(test, derive(PartialEq))]
    pub enum BinaryOp {
        Add,
        Sub,
        Mult,
        And,
        Or,
        Xor,
        SAL,
        SAR,
    }

    #[derive(Debug, Copy, Clone)]
    #[cfg_attr(test, derive(PartialEq))]
    pub enum CondCode {
        E,
        NE,
        G,
        GE,
        L,
        LE,
    }

    #[derive(Debug)]
    #[cfg_attr(test, derive(PartialEq))]
    pub enum Operand<S: State> {
        Imm(isize),
        Location(Location<S>),
    }
}
pub use ast::*;

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
impl<T, L, Input, Output> Identity<Location<Output>> for T
where
    Input: State<Location = L>,
    Output: State<Location = L>,
    T: Functor<Location<Output>, Input = Location<Input>>,
{
    type Mapped = T::Mapped;
    fn identity(self) -> Self::Mapped {
        self.fmap(|x| wrap(x.inner()))
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
