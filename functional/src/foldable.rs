use core::ops::Deref;

use crate::control_flow::RecursiveCall;

pub trait Foldable<A> {
    fn foldl_impl<'s, B>(
        &'s self,
        f: &mut impl FnMut(B, &'s A) -> B,
        acc: B,
        how: RecursiveCall,
    ) -> B
    where
        A: 's;

    #[inline(always)]
    fn foldl<'s, B>(&'s self, mut f: impl FnMut(B, &'s A) -> B, acc: B) -> B
    where
        A: 's,
    {
        self.foldl_impl(&mut f, acc, RecursiveCall::default())
    }
}

impl<T, A> Foldable<A> for Vec<T>
where
    T: Foldable<A>,
{
    fn foldl_impl<'s, B>(
        &'s self,
        f: &mut impl FnMut(B, &'s A) -> B,
        mut acc: B,
        how: RecursiveCall,
    ) -> B
    where
        A: 's,
    {
        for a in self.iter() {
            acc = a.foldl_impl(f, acc, how);
        }
        acc
    }
}

impl<T, A> Foldable<A> for Option<T>
where
    T: Foldable<A>,
{
    fn foldl_impl<'s, B>(
        &'s self,
        f: &mut impl FnMut(B, &'s A) -> B,
        acc: B,
        how: RecursiveCall,
    ) -> B
    where
        A: 's,
    {
        match self {
            Some(t) => t.foldl_impl(f, acc, how),
            None => acc,
        }
    }
}

impl<A, T> Foldable<A> for Box<T>
where
    T: Foldable<A>,
{
    fn foldl_impl<'s, B>(
        &'s self,
        f: &mut impl FnMut(B, &'s A) -> B,
        acc: B,
        how: RecursiveCall,
    ) -> B
    where
        A: 's,
    {
        self.deref().foldl_impl(f, acc, how)
    }
}

impl<T> Foldable<T> for () {
    fn foldl_impl<'s, B>(
        &'s self,
        _f: &mut impl FnMut(B, &'s T) -> B,
        acc: B,
        _how: RecursiveCall,
    ) -> B
    where
        T: 's,
    {
        acc
    }
}
