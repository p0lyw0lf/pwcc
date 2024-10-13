use functional::Functor;

use super::*;

#[derive(Debug)]
pub struct Pass;

impl State for Pass {
    type Location = hardware::Location;
}

fn pass<S: State<Location=pseudo::Location>>(instructions: Instructions<S>) -> Instructions<Pass> {
    Functor::<Location<Pass>>::fmap(instructions, &mut |loc: super::Location<S>| -> super::Location<Pass> {
        use pseudo::Location::*;
        wrap(match loc.inner() {
            Pseudo(i) => hardware::Location::Stack(4 * (i + 1)),
            Concrete(c) => c,
        })
    })
}
