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
