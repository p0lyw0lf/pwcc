pub trait Foldable<A> {
    fn foldl<B>(&self, f: &mut impl FnMut(B, &A) -> B, acc: B) -> B;
}

impl<A> Foldable<A> for Vec<A> {
    fn foldl<B>(&self, f: &mut impl FnMut(B, &A) -> B, mut acc: B) -> B {
        for a in self {
            acc = f(acc, a);
        }
        acc
    }
}

impl<A> Foldable<A> for Option<A> {
    fn foldl<B>(&self, f: &mut impl FnMut(B, &A) -> B, acc: B) -> B {
        match self {
            Some(a) => f(acc, a),
            None => acc,
        }
    }
}

#[macro_export]
macro_rules! foldable {
    (type$(<$($type:ident : $trait:ident),+>)? $base:path) => {
        impl<$($($type: $trait,)+)?> $crate::foldable::Foldable<$base$(<$($type,)+>)?> for $base$(<$($type,)+>)? {
            fn foldl<B>(&self, f: &mut impl FnMut(B, &$base) -> B, mut acc: B) -> B {
                f(acc, self)
            }
        }
    };
    (struct$(<$($type:ident : $trait:ident),+>)? $outer:ident for $inner:path {$(
        $field:ident,
    )+}) => {
        impl<$($($type: $trait,)+)?> $crate::foldable::Foldable<$inner> for $outer$(<$($type,)+>)? {
            fn foldl<B>(&self, f: &mut impl FnMut(B, &$inner) -> B, mut acc: B) -> B {
                $(acc = self.$field.foldl(f, acc);)+
                acc
            }
       }
    };
    (enum$(<$($type:ident : $trait:ident),+>)? $outer:ident for $inner:path { $(
        $case:ident { $($field:ident ,)+ } ,
    )+ }) => {
        impl<$($($type: $trait,)+)?> $crate::foldable::Foldable<$inner> for $outer$(<$($type,)+>)? {
            fn foldl<B>(&self, f: &mut impl FnMut(B, &$inner) -> B, mut acc: B) -> B {
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
