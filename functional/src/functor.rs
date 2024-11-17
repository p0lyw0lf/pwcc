pub trait Functor<Inner> {
    type Input;
    type Output;
    type Mapped;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped;
}

impl<T, Inner, A, B> Functor<Inner> for Vec<T>
where
    T: Functor<Inner, Input=A, Output=B>,
{
    type Input = A;
    type Output = B;
    type Mapped = Vec<T::Mapped>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
        self.into_iter().map(&mut |x: T| x.fmap(f)).collect()
    }
}

impl<T, Inner, A, B> Functor<Inner> for Option<T>
where
    T: Functor<Inner, Input=A, Output=B>,
{
    type Input = A;
    type Output = B;
    type Mapped = Option<T::Mapped>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
        Option::map(self, &mut |x: T| x.fmap(f))
    }
}
