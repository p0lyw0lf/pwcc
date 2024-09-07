use core::convert::From;

use super::*;

#[derive(Debug)]
pub struct Pass;

impl super::State for Pass {
    type Location = hardware::Location;
}

impl super::Pass for Pass {
    type PreviousLocation = pseudo::Location;

    fn pass(instructions: Instructions<Self::PreviousLocation>) -> Instructions<Self::Location> {
        instructions.into()
    }
}

impl From<pseudo::Location> for hardware::Location {
    fn from(loc: pseudo::Location) -> Self {
        use pseudo::Location::*;
        match loc {
            Pseudo(i) => hardware::Location::Stack(4 * (i + 1)),
            Concrete(c) => c,
        }
    }
}
