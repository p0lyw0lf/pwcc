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

#[derive(Debug)]
pub enum Reg {
    AX,
    DX,
    R10,
    R11,
}

impl super::Pass for Pass {
    type PreviousLocation = Location;
    fn pass(instructions: Instructions<Self::PreviousLocation>) -> Instructions<Self::Location> {
        let max_stack_addr = instructions.reduce(
            &mut |l: &Location, prev_max: usize| {
                use Location::*;
                match l {
                    Reg(_) => prev_max,
                    Stack(s) => core::cmp::max(*s, prev_max),
                }
            },
            0,
        );

        let instructions = instructions.0.into_iter().flat_map(
            |i| -> Box<dyn Iterator<Item = Instruction<Location>>> {
                use Instruction::*;
                match i {
                    Mov {
                        src: src @ Operand::Location(Location::Stack(_)),
                        dst: dst @ Location::Stack(_),
                    } => Box::new(
                        [
                            Mov {
                                src,
                                dst: Location::Reg(Reg::R10),
                            },
                            Mov {
                                src: Operand::Location(Location::Reg(Reg::R10)),
                                dst,
                            },
                        ]
                        .into_iter(),
                    ),
                    other => Box::new([other].into_iter()),
                }
            },
        );

        Instructions(
            core::iter::once(Instruction::AllocateStack {
                amount: max_stack_addr,
            })
            .chain(instructions)
            .collect(),
        )
    }
}
