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

pub trait Functor<Output> {
    type Input;
    type Mapped;

    fn fmap_impl(
        self,
        f: &mut impl FnMut(Self::Input) -> Output,
        how: RecursiveCall,
    ) -> Self::Mapped;

    #[inline(always)]
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Output) -> Self::Mapped
    where
        Self: Sized,
    {
        self.fmap_impl(f, RecursiveCall::default())
    }
}

impl<T, Input, Output> Functor<Output> for Vec<T>
where
    T: Functor<Output, Input = Input>,
{
    type Input = Input;
    type Mapped = Vec<T::Mapped>;
    fn fmap_impl(
        self,
        f: &mut impl FnMut(Self::Input) -> Output,
        how: RecursiveCall,
    ) -> Self::Mapped {
        self.into_iter()
            .map(&mut |x: T| x.fmap_impl(f, how))
            .collect()
    }
}

impl<T, Input, Output> Functor<Output> for Option<T>
where
    T: Functor<Output, Input = Input>,
{
    type Input = Input;
    type Mapped = Option<T::Mapped>;
    fn fmap_impl(
        self,
        f: &mut impl FnMut(Self::Input) -> Output,
        how: RecursiveCall,
    ) -> Self::Mapped {
        Option::map(self, &mut |x: T| x.fmap_impl(f, how))
    }
}

impl<T, Input, Output> Functor<Output> for Box<T>
where
    T: Functor<Output, Input = Input>,
{
    type Input = Input;
    type Mapped = Box<T::Mapped>;
    fn fmap_impl(
        self,
        f: &mut impl FnMut(Self::Input) -> Output,
        how: RecursiveCall,
    ) -> Self::Mapped {
        Box::new((*self).fmap_impl(f, how))
    }
}
