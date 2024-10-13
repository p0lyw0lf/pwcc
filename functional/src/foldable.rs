pub trait Foldable<A> {
    fn foldl<'s, B>(&'s self, f: fn(B, &'s A) -> B, acc: B) -> B
    where
        A: 's;
}

impl<T, A> Foldable<A> for Vec<T>
where
    T: Foldable<A>
{
    fn foldl<'s, B>(&'s self, f: fn(B, &'s A) -> B, mut acc: B) -> B
    where
        A: 's,
    {
        for a in self {
            acc = a.foldl(f, acc);
        }
        acc
    }
}

impl<A> Foldable<A> for Option<A> {
    fn foldl<'s, B>(&'s self, f: fn(B, &'s A) -> B, acc: B) -> B
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
    (type $base:ident $(<$($type:ident : $trait:ident),+>)?) => {
        impl<$($($type: $trait,)+)?> $crate::foldable::Foldable<$base$(<$($type,)+>)?> for $base$(<$($type,)+>)? {
            fn foldl<'s, B>(&'s self, f: fn(B, &'s $base$(<$($type,)+>)?) -> B, mut acc: B) -> B
            where
                $base$(<$($type,)+>)?: 's,
            {
                f(acc, self)
            }
        }
    };
    (struct $outer:ident $(<$($type:ident : $trait:ident),+>)? for $inner:path | {$(
        $field:ident,
    )+}) => {
        impl<$($($type: $trait,)+)?> $crate::foldable::Foldable<$inner> for $outer$(<$($type,)+>)? {
            fn foldl<'s, B>(&'s self, f: fn(B, &'s $inner) -> B, mut acc: B) -> B
            where
                $inner: 's,
            {
                $(acc = self.$field.foldl(f, acc);)+
                acc
            }
       }
    };
    (struct $outer:ident $(<$($type:ident : $trait:ident),+>)? for $inner:path | ($(
        $index:tt,
    )+)) => {
        impl<$($($type: $trait,)+)?> $crate::foldable::Foldable<$inner> for $outer$(<$($type,)+>)? {
            fn foldl<'s, B>(&'s self, f: fn(B, &'s $inner) -> B, mut acc: B) -> B
            where
                $inner: 's,
            {
                $(acc = self.$index.foldl(f, acc);)+
                acc
            }
       }
    };
    (enum $outer:ident $(<$($type:ident : $trait:ident),+>)? for $inner:path | { $(
        $case:ident
            $({ $($field:ident ,)+ })?
            $(( $($index:tt ,)+ ))?
        ,
    )+ }) => {
        impl<$($($type: $trait,)+)?> $crate::foldable::Foldable<$inner> for $outer$(<$($type,)+>)? {
            fn foldl<'s, B>(&'s self, f: fn(B, &'s $inner) -> B, mut acc: B) -> B
            where
                $inner: 's,
            {
                todo!()
            }
        }
    };
}
