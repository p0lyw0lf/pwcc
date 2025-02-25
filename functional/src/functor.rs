pub trait Functor<Output> {
    type Input;
    type Mapped;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Output) -> Self::Mapped;
}

impl<T, Input, Output> Functor<Output> for Vec<T>
where
    T: Functor<Output, Input = Input>,
{
    type Input = Input;
    type Mapped = Vec<T::Mapped>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Output) -> Self::Mapped {
        self.into_iter().map(&mut |x: T| x.fmap(f)).collect()
    }
}

impl<T, Input, Output> Functor<Output> for Option<T>
where
    T: Functor<Output, Input = Input>,
{
    type Input = Input;
    type Mapped = Option<T::Mapped>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Output) -> Self::Mapped {
        Option::map(self, &mut |x: T| x.fmap(f))
    }
}

impl<T, Input, Output> Functor<Output> for Box<T>
where
    T: Functor<Output, Input = Input>,
{
    type Input = Input;
    type Mapped = Box<T::Mapped>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Output) -> Self::Mapped {
        Box::new((*self).fmap(f))
    }
}
