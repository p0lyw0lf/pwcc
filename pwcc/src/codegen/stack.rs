use functional::functor::Functor;

use super::*;

#[derive(Debug)]
pub struct Pass;

impl State for Pass {
    type Location = hardware::Location;
}

fn pass<S: State<Location=pseudo::Location>>(instructions: Instructions<S>) -> Instructions<Pass> {
    instructions.map(&mut |loc: pseudo::Location| -> hardware::Location {
        use pseudo::Location::*;
        match loc {
            Pseudo(i) => hardware::Location::Stack(4 * (i + 1)),
            Concrete(c) => c,
        }
    })
}
