use super::*;

#[derive(Debug)]
pub struct Pass;

impl State for Pass {
    type Location = Location;
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Location {
    Reg(Reg),
    /// offset is relative to %rbp
    Stack(isize),
}

#[derive(Debug, Copy, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Reg {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
}

fn rax<S: State<Location = Location>>() -> super::Location<S> {
    wrap(Location::Reg(Reg::AX))
}

fn rcx<S: State<Location = Location>>() -> super::Location<S> {
    wrap(Location::Reg(Reg::CX))
}

fn r10d<S: State<Location = Location>>() -> super::Location<S> {
    wrap(Location::Reg(Reg::R10))
}

fn r11d<S: State<Location = Location>>() -> super::Location<S> {
    wrap(Location::Reg(Reg::R11))
}

pub fn pass<S: State<Location = Location>>(instructions: Instructions<S>) -> Instructions<Pass> {
    fn min_stack_addr<S: State<Location = Location>>(
        prev_min: isize,
        l: &super::Location<S>,
    ) -> isize {
        match l.as_ref() {
            Location::Reg(_) => prev_min,
            Location::Stack(s) => core::cmp::min(*s, prev_min),
        }
    }
    let min_stack_addr = instructions.foldl(min_stack_addr, 0);
    let required_stack_space = usize::try_from(-min_stack_addr)
        .unwrap()
        // Allocate stack space in multiples of 16 so that the stack is always aligned when we make
        // another function call.
        .next_multiple_of(16);

    let instructions =
        instructions
            .0
            .into_iter()
            .flat_map(|i| -> Box<dyn Iterator<Item = Instruction<Pass>>> {
                use Instruction::*;
                match i {
                    // Memory -> Memory moves are not allowed
                    Mov {
                        src: src @ Operand::Location(super::Location(Location::Stack(_))),
                        dst: dst @ super::Location(Location::Stack(_)),
                    } => {
                        let i1: Instruction<Pass> = Mov {
                            src: src.identity(),
                            dst: r10d(),
                        };
                        let i2: Instruction<Pass> = Mov {
                            src: Operand::Location(r10d()),
                            dst: dst.identity(),
                        };
                        Box::new([i1, i2].into_iter())
                    }
                    // Division must take Location as a operand
                    Idiv {
                        denom: dst @ Operand::Imm(_),
                    } => {
                        let i1: Instruction<Pass> = Mov {
                            src: dst.identity(),
                            dst: r10d(),
                        };
                        let i2: Instruction<Pass> = Idiv {
                            denom: Operand::Location(r10d()),
                        };
                        Box::new([i1, i2].into_iter())
                    }
                    // imull can't have dst be a memory location
                    Binary {
                        op: op @ BinaryOp::Mult,
                        src,
                        dst: dst @ super::Location(Location::Stack(_)),
                    } => {
                        let i1: Instruction<Pass> = Mov {
                            src: Operand::Location(dst.clone().identity()),
                            dst: r11d(),
                        };
                        let i2: Instruction<Pass> = Binary {
                            op,
                            src: src.identity(),
                            dst: r11d(),
                        };
                        let i3: Instruction<Pass> = Mov {
                            src: Operand::Location(r11d()),
                            dst: dst.identity(),
                        };
                        Box::new([i1, i2, i3].into_iter())
                    }
                    // sal/sar need to have special register as shift source
                    Binary {
                        op: op @ (BinaryOp::SAL | BinaryOp::SAR),
                        src,
                        dst,
                    } => {
                        // If it's already an immediate or the correct register, just pass through
                        if matches!(
                            src,
                            Operand::Location(super::Location(Location::Reg(Reg::CX)))
                                | Operand::Imm(_)
                        ) {
                            let i: Instruction<Pass> = Binary {
                                op,
                                src: src.identity(),
                                dst: dst.identity(),
                            };
                            Box::new([i].into_iter())
                        } else {
                            let i1: Instruction<Pass> = Mov {
                                src: src.identity(),
                                dst: rcx(),
                            };
                            let i2: Instruction<Pass> = Binary {
                                op,
                                src: Operand::Location(rcx()),
                                dst: dst.clone().identity(),
                            };
                            Box::new([i1, i2].into_iter())
                        }
                    }
                    // Memory -> Memory binary operations aren't allowed
                    Binary {
                        op,
                        src: src @ Operand::Location(super::Location(Location::Stack(_))),
                        dst: dst @ super::Location(Location::Stack(_)),
                    } => {
                        let i1: Instruction<Pass> = Mov {
                            src: src.identity(),
                            dst: r10d(),
                        };
                        let i2: Instruction<Pass> = Binary {
                            op,
                            src: Operand::Location(r10d()),
                            dst: dst.identity(),
                        };
                        Box::new([i1, i2].into_iter())
                    }
                    // Memory -> Memory cmp operations aren't allowed
                    Cmp {
                        left: left @ super::Location(Location::Stack(_)),
                        right: right @ Operand::Location(super::Location(Location::Stack(_))),
                    } => Box::new(
                        [
                            Mov {
                                src: Operand::Location(left.identity()),
                                dst: r10d(),
                            },
                            Cmp {
                                left: r10d(),
                                right: right.identity(),
                            },
                        ]
                        .into_iter(),
                    ),
                    // Direct pushes of memory aren't allowed for alignment reasons
                    Push(loc @ Operand::Location(super::Location(Location::Stack(_)))) => Box::new(
                        [
                            Mov {
                                src: loc.identity(),
                                dst: rax(),
                            },
                            Push(Operand::Location(rax())),
                        ]
                        .into_iter(),
                    ),
                    other => Box::new([other.identity()].into_iter()),
                }
            });

    Instructions(
        core::iter::once(Instruction::AllocateStack {
            amount: required_stack_space,
        })
        .chain(instructions)
        .collect(),
    )
}
