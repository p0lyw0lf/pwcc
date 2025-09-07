use super::*;

#[derive(Debug)]
pub struct Pass;

impl State for Pass {
    type Location = hardware::Location;
}

pub fn pass<S: State<Location = pseudo::Location>>(
    loc: super::Location<S>,
) -> super::Location<Pass> {
    use pseudo::Location::*;
    wrap(match loc.inner() {
        Pseudo(i) => hardware::Location::Stack(
            -std::convert::TryInto::<isize>::try_into(4 * (i + 1)).unwrap(),
        ),
        Concrete(c) => c,
    })
}
