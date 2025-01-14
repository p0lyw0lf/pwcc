/// A type with an associative binary operation, usually representing concatenation.
pub trait Semigroup {
    fn sconcat(self, other: Self) -> Self;
}

impl<T> Semigroup for Vec<T> {
    fn sconcat(mut self, other: Self) -> Self {
        self.extend(other);
        self
    }
}

impl<T: Semigroup> Semigroup for Option<T> {
    fn sconcat(self, other: Self) -> Self {
        match (self, other) {
            (a, None) => a,
            (None, b) => b,
            (Some(a), Some(b)) => Some(a.sconcat(b)),
        }
    }
}
