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
foldable!(type Location);

impl From<tacky::Program> for Program<State> {
    fn from(program: tacky::Program) -> Self {
        Self {
            function: program.function.into(),
        }
    }
}

impl From<tacky::Function> for Function<State> {
    fn from(function: tacky::Function) -> Self {
        Self {
            name: function.name.0,
            instructions: function.body.into(),
        }
    }
}

impl From<tacky::Instructions> for Instructions<State> {
    fn from(instructions: tacky::Instructions) -> Self {
        use tacky::Instruction::*;
        Self(
            instructions
                .0
                .into_iter()
                .flat_map(|i| {
                    let out: Box<dyn Iterator<Item = Instruction<State>>> = match i {
                        Return { val } => Box::new(
                            [
                                Instruction::Mov {
                                    src: val.into(),
                                    dst: wrap(Location::Concrete(hardware::Location::Reg(
                                        hardware::Reg::AX,
                                    ))),
                                },
                                Instruction::Ret,
                            ]
                            .into_iter(),
                        ),
                        Unary { op, src, dst } => Box::new(
                            [
                                Instruction::Mov {
                                    src: src.into(),
                                    dst: wrap(dst.into()),
                                },
                                Instruction::Unary {
                                    op: op.into(),
                                    dst: wrap(dst.into()),
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
                                    dst: wrap(Location::Concrete(hardware::Location::Reg(
                                        hardware::Reg::AX,
                                    ))),
                                },
                                Instruction::Cdq,
                                Instruction::Idiv { denom: src2.into() },
                                Instruction::Mov {
                                    src: Operand::Location(wrap(Location::Concrete(
                                        hardware::Location::Reg(
                                            if matches!(op, tacky::BinaryOp::Divide) {
                                                hardware::Reg::AX
                                            } else {
                                                hardware::Reg::DX
                                            },
                                        ),
                                    ))),
                                    dst: wrap(dst.into()),
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
                                    dst: wrap(dst.into()),
                                },
                                Instruction::Binary {
                                    op: match op {
                                        tacky::BinaryOp::Add => BinaryOp::Add,
                                        tacky::BinaryOp::Subtract => BinaryOp::Sub,
                                        tacky::BinaryOp::Multiply => BinaryOp::Mult,
                                        tacky::BinaryOp::BitAnd => BinaryOp::And,
                                        tacky::BinaryOp::BitOr => BinaryOp::Or,
                                        tacky::BinaryOp::BitXor => BinaryOp::Xor,
                                        tacky::BinaryOp::BitLeftShift => BinaryOp::SAL,
                                        tacky::BinaryOp::BitRightShift => BinaryOp::SAR,
                                        tacky::BinaryOp::Divide => unreachable!(),
                                        tacky::BinaryOp::Remainder => unreachable!(),
                                        tacky::BinaryOp::Equal => todo!(),
                                        tacky::BinaryOp::NotEqual => todo!(),
                                        tacky::BinaryOp::LessThan => todo!(),
                                        tacky::BinaryOp::LessThanEqual => todo!(),
                                        tacky::BinaryOp::GreaterThan => todo!(),
                                        tacky::BinaryOp::GreaterThanEqual => todo!(),
                                    },
                                    src: src2.into(),
                                    dst: wrap(dst.into()),
                                },
                            ]
                            .into_iter(),
                        ),
                        Copy { src, dst } => todo!(),
                        Jump { target } => todo!(),
                        JumpIfZero { condition, target } => todo!(),
                        JumpIfNotZero { condition, target } => todo!(),
                        Label(_) => todo!(),
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
            Not => todo!(),
        }
    }
}

impl From<tacky::Val> for Operand<State> {
    fn from(val: tacky::Val) -> Self {
        use tacky::Val::*;
        match val {
            Constant(i) => Operand::Imm(i),
            Var(t) => Operand::Location(wrap(t.into())),
        }
    }
}

impl From<tacky::Temporary> for Location {
    fn from(tmp: tacky::Temporary) -> Self {
        Location::Pseudo(tmp.0)
    }
}
