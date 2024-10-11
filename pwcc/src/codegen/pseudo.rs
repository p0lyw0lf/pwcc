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
            name: function.name,
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
                .flat_map(|i| {
                    let out: Box<dyn Iterator<Item = Instruction<Location>>> = match i {
                        Return { val } => Box::new(
                            [
                                Instruction::Mov {
                                    src: val.into(),
                                    dst: Location::Concrete(hardware::Location::Reg(
                                        hardware::Reg::AX,
                                    )),
                                },
                                Instruction::Ret,
                            ]
                            .into_iter(),
                        ),
                        Unary { op, src, dst } => Box::new(
                            [
                                Instruction::Mov {
                                    src: src.into(),
                                    dst: dst.into(),
                                },
                                Instruction::Unary {
                                    op: op.into(),
                                    dst: dst.into(),
                                },
                            ]
                            .into_iter(),
                        ),
                        Binary {
                            op: op @ (tacky::BinaryOp::Divide | tacky::BinaryOp::Remainder),
                            src1,
                            src2,
                            dst,
                        } => Box::new(
                            [
                                Instruction::Mov {
                                    src: src1.into(),
                                    dst: Location::Concrete(hardware::Location::Reg(
                                        hardware::Reg::AX,
                                    )),
                                },
                                Instruction::Cdq,
                                // TODO: make it impossible to represent a div acting directly on a
                                // constant
                                Instruction::Idiv { denom: src2.into() },
                                Instruction::Mov {
                                    src: Operand::Location(Location::Concrete(
                                        hardware::Location::Reg(
                                            if matches!(op, tacky::BinaryOp::Divide) {
                                                hardware::Reg::AX
                                            } else {
                                                hardware::Reg::DX
                                            },
                                        ),
                                    )),
                                    dst: dst.into(),
                                },
                            ]
                            .into_iter(),
                        ),
                        Binary {
                            op,
                            src1,
                            src2,
                            dst,
                        } => Box::new(
                            [
                                Instruction::Mov {
                                    src: src1.into(),
                                    dst: dst.into(),
                                },
                                Instruction::Binary {
                                    op: match op {
                                        tacky::BinaryOp::Add => BinaryOp::Add,
                                        tacky::BinaryOp::Subtract => BinaryOp::Sub,
                                        tacky::BinaryOp::Multiply => BinaryOp::Mult,
                                        tacky::BinaryOp::Divide => unreachable!(),
                                        tacky::BinaryOp::Remainder => unreachable!(),
                                    },
                                    src: src2.into(),
                                    dst: dst.into(),
                                },
                            ]
                            .into_iter(),
                        ),
                    };
                    out
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
