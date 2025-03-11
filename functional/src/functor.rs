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

    /// NOTE: `f` needs to be `&mut` so that we don't run into infinite type errors, for some
    /// reason.
    fn fmap_impl(
        self,
        f: &mut impl FnMut(Self::Input) -> Output,
        how: RecursiveCall,
    ) -> Self::Mapped;

    #[inline(always)]
    fn fmap(self, mut f: impl FnMut(Self::Input) -> Output) -> Self::Mapped
    where
        Self: Sized,
    {
        self.fmap_impl(&mut f, RecursiveCall::default())
    }
}

/// Generates a Functor implementation for a "base" wrapper newtype, like `Wrap<T>(T);`
/// The types MUST have that structure.
///
/// ```
/// struct Wrap<T>(T);
/// functional::functor_wrap!(Wrap);
/// ```
#[macro_export]
macro_rules! functor_wrap {
    ($wrap:ident) => {
        impl<T, Output> $crate::Functor<Output> for $wrap<T>
        where
            T: $crate::Functor<Output>,
        {
            type Input = T::Input;
            type Mapped = $wrap<T::Mapped>;
            fn fmap_impl(
                self,
                f: &mut impl FnMut(T::Input) -> Output,
                how: $crate::RecursiveCall,
            ) -> $wrap<T::Mapped> {
                $wrap(self.0.fmap_impl(f, how))
            }
        }
    };
}

/// Generates a Functor implementation for a type over itself. Used to fill in missing base
/// implementations for extern nodes in an AST.
///
/// ```
/// struct Base;
/// functional::functor_base!(Base);
/// ```
///
/// Simple generics are also allowed.
///
/// ```
/// struct Base<T>(T);
/// functional::functor_base!(Base<T>);
/// ```
#[macro_export]
macro_rules! functor_base {
    ($( $base:ident $(< $( $generic:ident ),* >)? ),* $(,)?) => {
        ::paste::paste! { $(

        impl$(<$([< $generic Input >], [< $generic Output >],)*>)? $crate::Functor<$base$(<$([< $generic Output >],)*>)?> for $base$(<$([< $generic Input >],)*>)? {
            type Input = Self;
            type Mapped = $base$(<$([< $generic Output >],)*>)?;

            fn fmap_impl(
                self,
                f: &mut impl FnMut(Self) -> $base$(<$([< $generic Output >],)*>)?,
                _how: $crate::RecursiveCall,
            ) -> $base$(<$([< $generic Output >],)*>)? {
                f(self)
            }
        }

        )* }
    }
}

impl<T, Output> Functor<Output> for Vec<T>
where
    T: Functor<Output>,
{
    type Input = T::Input;
    type Mapped = Vec<T::Mapped>;
    fn fmap_impl(
        self,
        f: &mut impl FnMut(Self::Input) -> Output,
        how: RecursiveCall,
    ) -> Self::Mapped {
        self.into_iter().map(|x: T| x.fmap_impl(f, how)).collect()
    }
}

impl<T, Output> Functor<Output> for Option<T>
where
    T: Functor<Output>,
{
    type Input = T::Input;
    type Mapped = Option<T::Mapped>;
    fn fmap_impl(
        self,
        f: &mut impl FnMut(T::Input) -> Output,
        how: RecursiveCall,
    ) -> Option<T::Mapped> {
        Option::map(self, |x: T| x.fmap_impl(f, how))
    }
}

impl<T, Output> Functor<Output> for Box<T>
where
    T: Functor<Output>,
{
    type Input = T::Input;
    type Mapped = Box<T::Mapped>;
    fn fmap_impl(
        self,
        f: &mut impl FnMut(Self::Input) -> Output,
        how: RecursiveCall,
    ) -> Box<T::Mapped> {
        Box::new((*self).fmap_impl(f, how))
    }
}
