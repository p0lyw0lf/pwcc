pub trait Functor<A, B> {
    type Output;
    fn map(self, f: &mut impl FnMut(A) -> B) -> Self::Output;
}

impl<A, B> Functor<A, B> for Vec<A> {
    type Output = Vec<B>;
    fn map(self, f: &mut impl FnMut(A) -> B) -> Self::Output {
        self.into_iter().map(f).collect()
    }
}

impl<A, B> Functor<A, B> for Option<A> {
    type Output = Option<B>;
    fn map(self, f: &mut impl FnMut(A) -> B) -> Self::Output {
        Option::map(self, f)
    }
}

#[macro_export]
macro_rules! functor {
    (type<$input_type:ident : $input_trait:ident, $output_type:ident : $output_trait:ident> $base:ident) => {
        impl<$input_type: $input_trait, $output_type: $output_trait> $crate::functor::Functor<$base<$input_type>, $base<$output_type>> for $base<$input_type> {
            type Output = $base<$output_type>;
            fn map(self, f: &mut impl FnMut(Self) -> $base<$output_type>) -> Self::Output {
                f(self)
            }
        }
    };
    (struct<$input_type:ident : $input_trait:ident, $output_type:ident : $output_trait:ident> $outer:ident for $input_inner:path |> $output_inner:path {
        $($field:ident,)+
        ..
        $($leftover:ident,)*
    }) => {
        impl<$input_type: $input_trait, $output_type: $output_trait> $crate::functor::Functor<$input_inner, $output_inner> for $outer<$input_type> {
            type Output = $outer<$output_type>;
            fn map(self, f: &mut impl FnMut($input_inner) -> $output_inner) -> Self::Output {
                todo!()
            }
        }
    };
    (enum<$input_type:ident : $input_trait:ident, $output_type:ident : $output_trait:ident> $outer:ident for $input_inner:path |> $output_inner:path { $(
        $case:ident {
            $($field:ident ,)+
            ..
            $($leftover:ident,)*
        } ,
    )+ }) => {
        impl<$input_type: $input_trait, $output_type: $output_trait> $crate::functor::Functor<$input_inner, $output_inner> for $outer<$input_type> {
            type Output = $outer<$output_type>;
            fn map(self, f: &mut impl FnMut($input_inner) -> $output_inner) -> Self::Output {
                todo!()
            }
        }
    };
}
