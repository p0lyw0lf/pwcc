use core::ops::Deref;

pub trait Foldable<A> {
    fn foldl<'s, B>(&'s self, f: fn(B, &'s A) -> B, acc: B) -> B
    where
        A: 's;
}

impl<T, A> Foldable<A> for Vec<T>
where
    T: Foldable<A>,
{
    fn foldl<'s, B>(&'s self, f: fn(B, &'s A) -> B, mut acc: B) -> B
    where
        A: 's,
    {
        for a in self.iter() {
            acc = a.foldl(f, acc);
        }
        acc
    }
}

impl<T, A> Foldable<A> for Option<T>
where
    T: Foldable<A>,
{
    fn foldl<'s, B>(&'s self, f: fn(B, &'s A) -> B, acc: B) -> B
    where
        A: 's,
    {
        match self {
            Some(t) => t.foldl(f, acc),
            None => acc,
        }
    }
}

impl<A, T> Foldable<A> for Box<T>
where
    T: Foldable<A>,
{
    fn foldl<'s, B>(&'s self, f: fn(B, &'s A) -> B, acc: B) -> B
    where
        A: 's,
    {
        self.deref().foldl(f, acc)
    }
}

impl<T> Foldable<T> for () {
    fn foldl<'s, B>(&'s self, _f: fn(B, &'s T) -> B, acc: B) -> B
    where
        T: 's,
    {
        acc
    }
}
