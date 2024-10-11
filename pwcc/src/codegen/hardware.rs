use super::*;

#[derive(Debug)]
pub struct Pass;

impl State for Pass {
    type Location = Location;
}

#[derive(Debug)]
pub enum Location {
    Reg(Reg),
    Stack(usize),
}
foldable!(type Location);

#[derive(Debug)]
pub enum Reg {
    AX,
    DX,
    R10,
    R11,
}

fn pass<S: State<Location = Location>>(instructions: Instructions<S>) -> Instructions<Pass> {
    fn count_stack_space(prev_max: usize, l: &super::Location<Pass>) -> usize {
        match l {
            super::Location(Location::Reg(_)) => prev_max,
            super::Location(Location::Stack(s)) => core::cmp::max(*s, prev_max),
        }
    }
    let max_stack_addr = instructions.foldl(&mut count_stack_space, 0);

    let instructions =
        instructions
            .0
            .into_iter()
            .flat_map(|i| -> Box<dyn Iterator<Item = Instruction<Pass>>> {
                use Instruction::*;
                match i {
                    Mov {
                        src:
                            src @ Operand::Location {
                                location: super::Location(Location::Stack(_)),
                            },
                        dst: dst @ super::Location(Location::Stack(_)),
                    } => {
                        let i1: Instruction<Pass> = Mov {
                            src: src.passthrough(),
                            dst: super::Location(Location::Reg(Reg::R10)),
                        };
                        let i2: Instruction<Pass> = Mov {
                            src: Operand::Location {
                                location: super::Location(Location::Reg(Reg::R10)),
                            },
                            dst: dst.passthrough(),
                        };
                        Box::new([i1, i2].into_iter())
                    }
                    other => Box::new([other.passthrough()].into_iter()),
                }
            });

    Instructions {
        instructions: core::iter::once(Instruction::AllocateStack {
            amount: max_stack_addr,
        })
        .chain(instructions)
        .collect(),
    }
}
