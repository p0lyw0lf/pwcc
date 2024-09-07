use core::convert::From;

use super::*;
use crate::tacky;

#[derive(Debug)]
pub struct State;

impl super::State for State {
    type Location = Location;
}

#[derive(Debug)]
pub enum Location {
    Pseudo(usize),
    Concrete(hardware::Location),
}

impl From<tacky::Program> for Program<State> {
    fn from(program: tacky::Program) -> Self {
        Self(program.function.into())
    }
}

impl From<tacky::Function> for Function<State> {
    fn from(function: tacky::Function) -> Self {
        Self {
            name: function.identifier,
            instructions: function.body.into(),
        }
    }
}

impl From<tacky::Instructions> for Instructions<Location> {
    fn from(instructions: tacky::Instructions) -> Self {
        use tacky::Instruction::*;
        Self(
            instructions
                .0
                .into_iter()
                .flat_map(|i| match i {
                    Return { val } => [
                        Instruction::Mov {
                            src: val.into(),
                            dst: Location::Concrete(hardware::Location::Reg(hardware::Reg::AX)),
                        },
                        Instruction::Ret,
                    ],
                    Unary { op, src, dst } => [
                        Instruction::Mov {
                            src: src.into(),
                            dst: dst.into(),
                        },
                        Instruction::Unary {
                            op: op.into(),
                            dst: dst.into(),
                        },
                    ],
                })
                .collect(),
        )
    }
}

impl From<tacky::UnaryOp> for UnaryOp {
    fn from(op: tacky::UnaryOp) -> Self {
        use tacky::UnaryOp::*;
        match op {
            Complement => UnaryOp::Not,
            Negate => UnaryOp::Neg,
        }
    }
}

impl From<tacky::Val> for Operand<Location> {
    fn from(val: tacky::Val) -> Self {
        use tacky::Val::*;
        match val {
            Constant(i) => Operand::Imm(i),
            Var(t) => Operand::Location(t.into()),
        }
    }
}

impl From<tacky::Temporary> for Location {
    fn from(tmp: tacky::Temporary) -> Self {
        Location::Pseudo(tmp.0)
    }
}
