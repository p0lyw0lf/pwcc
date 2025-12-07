use crate::Semigroup;
use crate::Tuple;

/// This trait makes dealing with tuples of `Semigroup` Results easier.
pub trait TryCoalesce<Output> {
    fn try_coalesce(self) -> Output;
}

impl<A, T, E> TryCoalesce<Result<T, E>> for A
where
    E: Semigroup,
    A: Into<Result<T, E>>,
{
    fn try_coalesce(self) -> Result<T, E> {
        self.into()
    }
}

impl<A, B, T1, T2, E> TryCoalesce<Result<Tuple<T1, T2>, E>> for Tuple<A, B>
where
    E: Semigroup,
    A: TryCoalesce<Result<T1, E>>,
    B: Into<Result<T2, E>>,
{
    fn try_coalesce(self) -> Result<Tuple<T1, T2>, E> {
        let Tuple { fst: a, snd: b } = self;
        let (a, b) = (a.try_coalesce(), b.into());

        match (a, b) {
            (Ok(a), Ok(b)) => Ok(Tuple { fst: a, snd: b }),
            (Err(a), Ok(_)) => Err(a),
            (Ok(_), Err(b)) => Err(b),
            (Err(mut a), Err(b)) => {
                a.sconcat(b);
                Err(a)
            }
        }
    }
}
