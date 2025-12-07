use crate::Tuple;

/// A type with an associative binary operation, usually representing concatenation.
pub trait Semigroup {
    fn sconcat(&mut self, other: Self);
}

impl<T: Semigroup> Semigroup for Box<T> {
    fn sconcat(&mut self, other: Self) {
        T::sconcat(self, *other)
    }
}

impl<T: Semigroup> Semigroup for Option<T> {
    fn sconcat(&mut self, other: Self) {
        // Can't use a match statement here because it moves out of `self`, which we don't want.
        if self.is_none() {
            *self = other;
            return;
        }
        if other.is_none() {
            return;
        }

        match (self, other) {
            (Some(a), Some(b)) => {
                a.sconcat(b);
            }
            _ => unreachable!(),
        }
    }
}

impl<T> Semigroup for Vec<T> {
    fn sconcat(&mut self, other: Self) {
        self.extend(other);
    }
}

impl Semigroup for () {
    fn sconcat(&mut self, _other: Self) {}
}

impl<A, B> Semigroup for Tuple<A, B>
where
    A: Semigroup,
    B: Semigroup,
{
    fn sconcat(&mut self, other: Self) {
        self.fst.sconcat(other.fst);
        self.snd.sconcat(other.snd);
    }
}

/// Lets the `Err` case take precedence over the `Ok` case.
impl<T, E> Semigroup for Result<T, E>
where
    T: Semigroup,
    E: Semigroup,
{
    fn sconcat(&mut self, other: Self) {
        match (self, other) {
            (Ok(va), Ok(vb)) => {
                va.sconcat(vb);
            }
            (Err(ea), Err(eb)) => {
                ea.sconcat(eb);
            }
            (a, Err(eb)) => {
                *a = Err(eb);
            }
            (_a, Ok(_vb)) => {
                // Do nothing; existing error overrides the new value
            }
        }
    }
}
