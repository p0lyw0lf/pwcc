use crate::ControlFlow;
use crate::Functor;
use crate::RecursiveCall;
use crate::Semigroup;

/// Follows the same pattern as Functor, though this time with the ability to collect all errors
/// that it encounters into one big error before returning a transformed output.
pub trait TryFunctor<Output>: Functor<Output> {
    fn try_fmap_impl<E: Semigroup + ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Output, E>,
        how: RecursiveCall,
    ) -> Result<Self::Mapped, E>;

    #[inline(always)]
    fn try_fmap<E: Semigroup + ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Output, E>,
    ) -> Result<Self::Mapped, E>
    where
        Self: Sized,
    {
        self.try_fmap_impl(f, RecursiveCall::default())
    }
}

impl<T, Output> TryFunctor<Output> for Vec<T>
where
    T: TryFunctor<Output>,
{
    fn try_fmap_impl<E: Semigroup + ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Output, E>,
        how: RecursiveCall,
    ) -> Result<Self::Mapped, E> {
        // Strategy: collect all errors, return concatenation
        let mut err = Option::<E>::None;
        let mut output = Vec::with_capacity(self.len());

        for x in self.into_iter() {
            match x.try_fmap_impl(f, how) {
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

impl<T, Output> TryFunctor<Output> for Option<T>
where
    T: TryFunctor<Output>,
{
    fn try_fmap_impl<E: Semigroup + ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Output, E>,
        how: RecursiveCall,
    ) -> Result<Self::Mapped, E> {
        match self {
            Some(v) => v.try_fmap_impl(f, how).map(Some),
            None => Ok(None),
        }
    }
}

impl<T, Output> TryFunctor<Output> for Box<T>
where
    T: TryFunctor<Output>,
{
    fn try_fmap_impl<E: Semigroup + ControlFlow>(
        self,
        f: &mut impl FnMut(Self::Input) -> Result<Output, E>,
        how: RecursiveCall,
    ) -> Result<Self::Mapped, E> {
        Ok(Box::new((*self).try_fmap_impl(f, how)?))
    }
}

/// Generates a TryFunctor implementation for a "base" wrapper newtype, like `Wrap<T>(T);`
/// The types MUST have that structure.
///
/// ```
/// struct Wrap<T>(T);
/// functional::functor_wrap!(Wrap);
/// functional::try_functor_wrap!(Wrap);
/// ```
#[macro_export]
macro_rules! try_functor_wrap {
    ($wrap:ident) => {
        impl<T, Output> $crate::TryFunctor<Output> for $wrap<T>
        where
            T: $crate::TryFunctor<Output>,
        {
            fn try_fmap_impl<E: $crate::Semigroup + $crate::ControlFlow>(
                self,
                f: &mut impl FnMut(T::Input) -> Result<Output, E>,
                how: $crate::RecursiveCall,
            ) -> Result<$wrap<T::Mapped>, E> {
                Ok($wrap(self.0.try_fmap_impl(f, how)?))
            }
        }
    };
}

/// Generates a TryFunctor implementation for a type over itself. Used to fill in missing base
/// implementations for extern nodes in an AST.
///
/// ```
/// struct Base;
/// functional::functor_base!(Base);
/// functional::try_functor_base!(Base);
/// ```
///
/// Simple generics are also allowed.
///
/// ```
/// struct Base<T>(T);
/// functional::functor_base!(Base<T>);
/// functional::try_functor_base!(Base<T>);
/// ```
#[macro_export]
macro_rules! try_functor_base {
    ($( $base:ident $(< $( $generic:ident ),* >)? ),* $(,)?) => {
        ::paste::paste! { $(

        impl$(<$([< $generic Input >], [< $generic Output >],)*>)? $crate::TryFunctor<$base$(<$([< $generic Output >],)*>)?> for $base$(<$([< $generic Input >],)*>)? {
            fn try_fmap_impl<E: $crate::Semigroup + $crate::ControlFlow>(
                self,
                f: &mut impl FnMut(Self) -> Result<$base$(<$([< $generic Output >],)*>)?, E>,
                _how: $crate::RecursiveCall,
            ) -> Result<$base$(<$([< $generic Output >],)*>)?, E> {
                f(self)
            }
        }

        )* }
    }
}
