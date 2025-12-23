use std::convert::From;
use std::iter;

use super::*;
use crate::tacky;

#[cfg(test)]
mod test;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct State;

impl super::State for State {
    type Location = Location;
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Location {
    /// offset is the virtual register representing this location
    Pseudo(usize),
    Concrete(hardware::Location),
}

const ARG_REGISTERS: [hardware::Reg; 6] = {
    use hardware::Reg::*;
    [DI, SI, DX, CX, R8, R9]
};

impl From<tacky::Program> for Program<State> {
    fn from(program: tacky::Program) -> Self {
        Self {
            declarations: program.declarations.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<tacky::Declaration> for Declaration<State> {
    fn from(decl: tacky::Declaration) -> Self {
        match decl {
            tacky::Declaration::Function(f) => Declaration::Function(f.into()),
            tacky::Declaration::StaticVariable(s) => Declaration::StaticVariable(s.into()),
        }
    }
}

impl From<tacky::StaticVariable> for StaticVariable {
    fn from(var: tacky::StaticVariable) -> Self {
        Self {
            name: var.name,
            global: var.global,
            initial_value: var.initial_value,
        }
    }
}

impl From<tacky::Function> for Function<State> {
    fn from(function: tacky::Function) -> Self {
        let caller_args = ARG_REGISTERS
            .iter()
            .map(|reg| hardware::Location::Reg(*reg))
            .chain(iter::successors(Some(16), |n| Some(n + 8)).map(hardware::Location::Stack))
            .map(Location::Concrete)
            .map(wrap::<State>)
            .map(Operand::Location);

        let preamble = function
            .args
            .into_iter()
            .zip(caller_args)
            .map(|(arg, loc)| Instruction::Mov {
                src: loc,
                dst: wrap(arg.into()),
            });

        let body: Instructions<State> = function.body.into();

        Self {
            name: function.name,
            global: function.global,
            instructions: Instructions(preamble.chain(body.0).collect()),
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
                        Unary {
                            op: tacky::UnaryOp::Not,
                            src,
                            dst,
                        } => Box::new(
                            [
                                Instruction::Mov {
                                    src: src.into(),
                                    dst: wrap(dst.into()),
                                },
                                Instruction::Cmp {
                                    left: wrap(dst.into()),
                                    right: Operand::Imm(0),
                                },
                                Instruction::Mov {
                                    src: Operand::Imm(0),
                                    dst: wrap(dst.into()),
                                },
                                Instruction::SetCC(CondCode::E, wrap(dst.into())),
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
                                    op: match op {
                                        tacky::UnaryOp::Complement => UnaryOp::Not,
                                        tacky::UnaryOp::Negate => UnaryOp::Neg,
                                        tacky::UnaryOp::Not => unreachable!(),
                                    },
                                    dst: wrap(dst.into()),
                                },
                            ]
                            .into_iter(),
                        ),
                        Binary {
                            op:
                                op @ (tacky::BinaryOp::Equal
                                | tacky::BinaryOp::NotEqual
                                | tacky::BinaryOp::LessThan
                                | tacky::BinaryOp::LessThanEqual
                                | tacky::BinaryOp::GreaterThan
                                | tacky::BinaryOp::GreaterThanEqual),
                            src1,
                            src2,
                            dst,
                        } => Box::new(
                            [
                                Instruction::Mov {
                                    src: src1.into(),
                                    dst: wrap(dst.into()),
                                },
                                Instruction::Cmp {
                                    left: wrap(dst.into()),
                                    right: src2.into(),
                                },
                                Instruction::Mov {
                                    src: Operand::Imm(0),
                                    dst: wrap(dst.into()),
                                },
                                Instruction::SetCC(
                                    match op {
                                        tacky::BinaryOp::Equal => CondCode::E,
                                        tacky::BinaryOp::NotEqual => CondCode::NE,
                                        tacky::BinaryOp::LessThan => CondCode::L,
                                        tacky::BinaryOp::LessThanEqual => CondCode::LE,
                                        tacky::BinaryOp::GreaterThan => CondCode::G,
                                        tacky::BinaryOp::GreaterThanEqual => CondCode::GE,
                                        _ => unreachable!(),
                                    },
                                    wrap(dst.into()),
                                ),
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
                                        tacky::BinaryOp::Divide | tacky::BinaryOp::Remainder => {
                                            unreachable!()
                                        }
                                        tacky::BinaryOp::Equal
                                        | tacky::BinaryOp::NotEqual
                                        | tacky::BinaryOp::LessThan
                                        | tacky::BinaryOp::LessThanEqual
                                        | tacky::BinaryOp::GreaterThan
                                        | tacky::BinaryOp::GreaterThanEqual => unreachable!(),
                                    },
                                    src: src2.into(),
                                    dst: wrap(dst.into()),
                                },
                            ]
                            .into_iter(),
                        ),
                        Copy { src, dst } => Box::new(
                            [Instruction::Mov {
                                src: src.into(),
                                dst: wrap(dst.into()),
                            }]
                            .into_iter(),
                        ),
                        Jump { target } => Box::new([Instruction::Jmp(target)].into_iter()),
                        JumpIfZero { condition, target } => Box::new(
                            [
                                Instruction::Cmp {
                                    left: wrap(condition.into()),
                                    right: Operand::Imm(0),
                                },
                                Instruction::JmpCC(CondCode::E, target),
                            ]
                            .into_iter(),
                        ),
                        JumpIfNotZero { condition, target } => Box::new(
                            [
                                Instruction::Cmp {
                                    left: wrap(condition.into()),
                                    right: Operand::Imm(0),
                                },
                                Instruction::JmpCC(CondCode::NE, target),
                            ]
                            .into_iter(),
                        ),
                        Label(identifier) => Box::new([Instruction::Label(identifier)].into_iter()),
                        Call {
                            name,
                            mut args,
                            dst,
                        } => {
                            let mut instructions = Vec::<Instruction<State>>::new();

                            let stack_args = args
                                .drain(std::cmp::min(args.len(), ARG_REGISTERS.len())..)
                                .rev();
                            let num_stack_args = stack_args.len();

                            let stack_padding = (num_stack_args % 2) * 8;
                            if stack_padding != 0 {
                                instructions.push(Instruction::AllocateStack {
                                    amount: stack_padding,
                                });
                            }

                            for arg in stack_args {
                                instructions.push(Instruction::Push(arg.into()));
                            }

                            let register_args = args.into_iter().zip(
                                ARG_REGISTERS
                                    .iter()
                                    .map(|reg| hardware::Location::Reg(*reg)),
                            );
                            for (arg, reg) in register_args {
                                instructions.push(Instruction::Mov {
                                    src: arg.into(),
                                    dst: wrap(Location::Concrete(reg)),
                                });
                            }

                            instructions.push(Instruction::Call(name));

                            let bytes_to_remove = 8 * num_stack_args + stack_padding;
                            if bytes_to_remove != 0 {
                                instructions.push(Instruction::DeallocateStack {
                                    amount: bytes_to_remove,
                                });
                            }

                            instructions.push(Instruction::Mov {
                                // TODO: this is a very ugly way to say "%rax". I should probably
                                // add more conversion helpers...
                                // Maybe also want to simplify the conversion helpers somehow too?
                                // There's a lot of ugliness elsewhere too ya
                                src: Operand::Location(wrap(Location::Concrete(
                                    hardware::Location::Reg(hardware::Reg::AX),
                                ))),
                                dst: wrap(dst.into()),
                            });

                            Box::new(instructions.into_iter())
                        }
                    };
                    out
                })
                .collect(),
        )
    }
}

impl From<tacky::Val> for Operand<State> {
    fn from(val: tacky::Val) -> Self {
        use tacky::Val::*;
        match val {
            Constant(i) => Operand::Imm(i),
            Var(t) => Operand::Location(wrap(t.into())),
            Data(d) => Operand::Location(wrap(d.into())),
        }
    }
}

impl From<tacky::Temporary> for Location {
    fn from(tmp: tacky::Temporary) -> Self {
        Location::Pseudo(tmp.0)
    }
}

impl From<tacky::Identifier> for Location {
    fn from(label: tacky::Identifier) -> Self {
        Location::Concrete(hardware::Location::Data(label))
    }
}
