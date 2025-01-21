pub trait ControlFlow {
    // Short for "continue", represents whether a given value should allow computation to continue
    // once it has been encountered.
    fn cont(&self) -> bool {
        false
    }
}

impl ControlFlow for () {}
