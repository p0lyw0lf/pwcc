pub trait Foldable<A> {
    fn foldl<'s, 'f: 's, B>(&'s self, f: &'f mut impl FnMut(B, &'s A) -> B, acc: B) -> B
    where
        A: 's;
}

impl<A> Foldable<A> for Vec<A> {
    fn foldl<'s, 'f: 's, B>(&'s self, f: &'f mut impl FnMut(B, &'s A) -> B, mut acc: B) -> B
    where
        A: 's,
    {
        for a in self {
            acc = f(acc, a);
        }
        acc
    }
}

impl<A> Foldable<A> for Option<A> {
    fn foldl<'s, 'f: 's, B>(&'s self, f: &'f mut impl FnMut(B, &'s A) -> B, acc: B) -> B
    where
        A: 's,
    {
        match self {
            Some(a) => f(acc, a),
            None => acc,
        }
    }
}

#[macro_export]
macro_rules! foldable {
    (type$(<$($type:ident : $trait:ident),+>)? $base:ident) => {
        impl<$($($type: $trait,)+)?> $crate::foldable::Foldable<$base$(<$($type,)+>)?> for $base$(<$($type,)+>)? {
    fn foldl<'s, 'f: 's, B>(&'s self, f: &'f mut impl FnMut(B, &'s $base$(<$($type,)+>)?) -> B, mut acc: B) -> B
    where
        $base$(<$($type,)+>)?: 's,
    {
                f(acc, self)
            }
        }
    };
    (struct$(<$($type:ident : $trait:ident),+>)? $outer:ident for $inner:path {$(
        $field:ident,
    )+}) => {
        impl<$($($type: $trait,)+)?> $crate::foldable::Foldable<$inner> for $outer$(<$($type,)+>)? {
    fn foldl<'s, 'f: 's, B>(&'s self, f: &'f mut impl FnMut(B, &'s $inner) -> B, mut acc: B) -> B
    where
        $inner: 's,
    {
                $(acc = self.$field.foldl(f, acc);)+
                acc
            }
       }
    };
    (enum$(<$($type:ident : $trait:ident),+>)? $outer:ident for $inner:path { $(
        $case:ident { $($field:ident ,)+ } ,
    )+ }) => {
        impl<$($($type: $trait,)+)?> $crate::foldable::Foldable<$inner> for $outer$(<$($type,)+>)? {
    fn foldl<'s, 'f: 's, B>(&'s self, f: &'f mut impl FnMut(B, &'s $inner) -> B, mut acc: B) -> B
    where
        $inner: 's,
    {
                match self {$(
                    Self::$case { $($field,)+ .. } => {
                        $(acc = $field.foldl(f, acc);)+
                        acc
                    }
                )+
                    _ => acc,
                }
            }
        }
    };
}
