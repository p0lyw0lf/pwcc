use core::fmt::Debug;

pub mod hardware;
pub mod pseudo;
pub mod stack;

pub trait State {
    type Location: Sized + Debug;
}

pub trait Pass: State {
    type PreviousLocation: Sized + Debug;
    fn pass(instructions: Instructions<Self::PreviousLocation>) -> Instructions<Self::Location>;
}

trait Reduce<T> {
    fn reduce<Acc>(&self, f: &mut impl FnMut(&T, Acc) -> Acc, acc: Acc) -> Acc;
}

#[derive(Debug)]
pub struct Program<S: State>(pub Function<S>);

impl<P1: State> Program<P1> {
    pub fn run_pass<P2>(self) -> Program<P2>
    where
        P2: Pass<PreviousLocation = P1::Location>,
    {
        Program(self.0.into())
    }
}

impl<S: State> Reduce<S::Location> for Program<S> {
    fn reduce<Acc>(&self, f: &mut impl FnMut(&S::Location, Acc) -> Acc, acc: Acc) -> Acc {
        self.0.reduce(f, acc)
    }
}

#[derive(Debug)]
pub struct Function<S: State> {
    pub name: String,
    pub instructions: Instructions<S::Location>,
}

impl<P1: State> Function<P1> {
    fn into<P2>(self) -> Function<P2>
    where
        P2: Pass<PreviousLocation = P1::Location>,
    {
        Function {
            name: self.name,
            instructions: P2::pass(self.instructions),
        }
    }
}

impl<S: State> Reduce<S::Location> for Function<S> {
    fn reduce<Acc>(&self, f: &mut impl FnMut(&S::Location, Acc) -> Acc, acc: Acc) -> Acc {
        self.instructions.reduce(f, acc)
    }
}

#[derive(Debug)]
pub struct Instructions<Location>(pub Vec<Instruction<Location>>);

impl<L1> Instructions<L1> {
    fn into<L2>(self) -> Instructions<L2>
    where
        L1: Into<L2>,
    {
        Instructions(self.0.into_iter().map(Instruction::into).collect())
    }
}

impl<L> Reduce<L> for Instructions<L> {
    fn reduce<Acc>(&self, f: &mut impl FnMut(&L, Acc) -> Acc, mut acc: Acc) -> Acc {
        for instruction in &self.0 {
            acc = instruction.reduce(f, acc);
        }
        acc
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Instruction<Location> {
    Mov {
        src: Operand<Location>,
        dst: Location,
    },
    Unary {
        op: UnaryOp,
        dst: Location,
    },
    Binary {
        op: BinaryOp,
        src: Operand<Location>,
        dst: Location,
    },
    Idiv {
        denom: Operand<Location>,
    },
    Cdq,
    AllocateStack {
        amount: usize,
    },
    Ret,
}

// TODO: write a macro for this (only going to get larger)
impl<L1> Instruction<L1> {
    fn into<L2>(self) -> Instruction<L2>
    where
        L1: Into<L2>,
    {
        use Instruction::*;
        match self {
            Mov { src, dst } => Mov {
                src: src.into(),
                dst: dst.into(),
            },
            Unary { op, dst } => Unary {
                op,
                dst: dst.into(),
            },
            Binary { op, src, dst } => Binary {
                op,
                src: src.into(),
                dst: dst.into(),
            },
            Idiv { denom } => Idiv { denom: denom.into() },
            Cdq => Cdq,
            AllocateStack { amount } => AllocateStack { amount },
            Ret => Ret,
        }
    }
}

// TODO: write a macro for this one too
impl<L> Reduce<L> for Instruction<L> {
    fn reduce<Acc>(&self, f: &mut impl FnMut(&L, Acc) -> Acc, acc: Acc) -> Acc {
        use Instruction::*;
        match self {
            Mov { src, dst } => {
                let acc = src.reduce(f, acc);
                let acc = f(dst, acc);
                acc
            }
            Unary { op: _, dst } => f(dst, acc),
            Binary { op: _, src, dst } => {
                let acc = src.reduce(f, acc);
                let acc = f(dst, acc);
                acc
            }
            Idiv { denom } => denom.reduce(f, acc),
            Cdq => acc,
            AllocateStack { .. } => acc,
            Ret => acc,
        }
    }
}

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

#[derive(Debug, Copy, Clone)]
pub enum Operand<Location> {
    Imm(isize),
    Location(Location),
}

impl<L1> Operand<L1> {
    fn into<L2>(self) -> Operand<L2>
    where
        L1: Into<L2>,
    {
        use Operand::*;
        match self {
            Imm(i) => Imm(i),
            Location(l) => Location(l.into()),
        }
    }
}

impl<L> Reduce<L> for Operand<L> {
    fn reduce<Acc>(&self, f: &mut impl FnMut(&L, Acc) -> Acc, acc: Acc) -> Acc {
        use Operand::*;
        match self {
            Imm(_) => acc,
            Location(l) => f(l, acc),
        }
    }
}
