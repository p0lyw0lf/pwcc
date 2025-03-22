pub trait ControlFlow {
    // Short for "continue", represents whether a given value should allow computation to continue
    // once it has been encountered.
    fn cont(&self) -> bool {
        false
    }
}

impl ControlFlow for () {}

/// If there is a functor implementation for a recursive datatype, where should the call to f(self)
/// go?
#[derive(Copy, Clone, Default, Eq, PartialEq)]
pub enum RecursiveCall {
    /// Calls f(self), then recurses on that value. Somewhat equivalent to writing a recursive
    /// descent parser yourself.
    Begin,
    /// Does not recurse on self, just calls f(self) and returns
    None,
    /// Recurses on self, then calls f(out). Equivalent to using biplate.
    #[default]
    End,
}
