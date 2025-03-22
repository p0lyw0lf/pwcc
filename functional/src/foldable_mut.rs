use core::ops::DerefMut;

use crate::control_flow::RecursiveCall;

pub trait FoldableMut<A> {
    fn foldl_mut_impl<B>(
        &mut self,
        f: &mut impl FnMut(B, &mut A) -> B,
        acc: B,
        how: RecursiveCall,
    ) -> B;

    #[inline(always)]
    fn foldl_mut<B>(&mut self, mut f: impl FnMut(B, &mut A) -> B, acc: B) -> B {
        self.foldl_mut_impl(&mut f, acc, RecursiveCall::default())
    }
}

impl<T, A> FoldableMut<A> for Vec<T>
where
    T: FoldableMut<A>,
{
    fn foldl_mut_impl<B>(
        &mut self,
        f: &mut impl FnMut(B, &mut A) -> B,
        mut acc: B,
        how: RecursiveCall,
    ) -> B {
        for a in self.iter_mut() {
            acc = a.foldl_mut_impl(f, acc, how);
        }
        acc
    }
}

impl<T, A> FoldableMut<A> for Option<T>
where
    T: FoldableMut<A>,
{
    fn foldl_mut_impl<B>(
        &mut self,
        f: &mut impl FnMut(B, &mut A) -> B,
        acc: B,
        how: RecursiveCall,
    ) -> B {
        match self {
            Some(t) => t.foldl_mut_impl(f, acc, how),
            None => acc,
        }
    }
}

impl<A, T> FoldableMut<A> for Box<T>
where
    T: FoldableMut<A>,
{
    fn foldl_mut_impl<B>(
        &mut self,
        f: &mut impl FnMut(B, &mut A) -> B,
        acc: B,
        how: RecursiveCall,
    ) -> B {
        self.deref_mut().foldl_mut_impl(f, acc, how)
    }
}

impl<T> FoldableMut<T> for () {
    fn foldl_mut_impl<B>(
        &mut self,
        _f: &mut impl FnMut(B, &mut T) -> B,
        acc: B,
        _how: RecursiveCall,
    ) -> B {
        acc
    }
}
