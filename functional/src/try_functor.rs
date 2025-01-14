use crate::ControlFlow;
use crate::Functor;
use crate::Semigroup;

pub trait TryFunctor<Inner>: Functor<Inner> {
    fn try_fmap<E: Semigroup + ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Self::Output, E>,
    ) -> Result<Self::Mapped, E>;
}

impl<T, Inner, A, B> TryFunctor<Inner> for Vec<T>
where
    T: TryFunctor<Inner, Input = A, Output = B>,
{
    fn try_fmap<E: Semigroup + ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Self::Output, E>,
    ) -> Result<Self::Mapped, E> {
        // Strategy: collect all errors, return concatenation
        let mut err = Option::<E>::None;
        let mut output = Vec::with_capacity(self.len());

        for x in self.into_iter() {
            match x.try_fmap(f) {
                Ok(v) => output.push(v),
                Err(e) => {
                    // SAFETY: None.sconcat(Some(e)) is always Some
                    let new_err = unsafe { err.sconcat(Some(e)).unwrap_unchecked() };
                    if !new_err.cont() {
                        return Err(new_err);
                    }
                    err = Some(new_err);
                }
            }
        }

        match err {
            Some(e) => Err(e),
            None => Ok(output),
        }
    }
}

impl<T, Inner, A, B> TryFunctor<Inner> for Option<T>
where
    T: TryFunctor<Inner, Input = A, Output = B>,
{
    fn try_fmap<E: Semigroup + ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Self::Output, E>,
    ) -> Result<Self::Mapped, E> {
        match self {
            Some(v) => v.try_fmap(f).map(Some),
            None => Ok(None),
        }
    }
}


impl<T, Inner, A, B> TryFunctor<Inner> for Box<T>
where
    T: TryFunctor<Inner, Input = A, Output = B>,
{
    fn try_fmap<E: Semigroup + ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Self::Output, E>,
    ) -> Result<Self::Mapped, E> {
        Ok(Box::new((*self).try_fmap(f)?))
    }
}
