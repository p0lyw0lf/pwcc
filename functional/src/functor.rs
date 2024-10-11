pub trait Functor<B> {
    type Input;
    type Output;
    type Mapped: Functor<B, Input=B, Mapped=Self::Mapped> + Functor<Self::Input, Input=B, Mapped=Self>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped;
}

impl<A, B> Functor<B> for Vec<A> {
    type Input = A;
    type Output = B;
    type Mapped = Vec<B>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
        self.into_iter().map(f).collect()
    }
}

impl<A, B> Functor<B> for Option<A> {
    type Input = A;
    type Output = B;
    type Mapped = Option<B>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
        Option::map(self, f)
    }
}

#[macro_export]
macro_rules! functor {
    (type<$input_type:ident : $input_trait:ident, $output_type:ident : $output_trait:ident> $base:ident) => {
        impl<$input_type: $input_trait, $output_type: $output_trait> $crate::functor::Functor<$base<$output_type>> for $base<$input_type> {
            type Input = $base<$input_type>;
            type Output = $base<$output_type>;
            type Mapped = $base<$output_type>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                f(self)
            }
        }
    };
    (struct<$input_type:ident : $input_trait:ident, $output_type:ident : $output_trait:ident> $outer:ident for $input_inner:path |> $output_inner:path {
        $($field:ident,)+
        ..
        $($other_field:ident,)*
    }) => {
        impl<$input_type: $input_trait, $output_type: $output_trait> $crate::functor::Functor<$output_inner> for $outer<$input_type> {
            type Input = $input_inner;
            type Output = $output_inner;
            type Mapped = $outer<$output_type>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                Self::Mapped {
                    $($field: $crate::functor::Functor::<$output_inner>::fmap(self.$field, f),)+
                    $($other_field: self.$other_field,)*
                }
            }
        }
    };
    (enum<$input_type:ident : $input_trait:ident, $output_type:ident : $output_trait:ident> $outer:ident for $input_inner:path |> $output_inner:path { $(
        $case:ident {
            $($field:ident,)*
            ..
            $($other_field:ident,)*
        } ,
    )+ }) => {
        impl<$input_type: $input_trait, $output_type: $output_trait> $crate::functor::Functor<$output_inner> for $outer<$input_type> {
            type Input = $input_inner;
            type Output = $output_inner;
            type Mapped = $outer<$output_type>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                match self {
                $(
                    Self::$case {
                        $($field,)*
                        $($other_field,)*
                    } => Self::Mapped::$case {
                        $($field: $crate::functor::Functor::<$output_inner>::fmap($field, f),)*
                        $($other_field,)*
                    },
                )+
                }
            }
        }
    };
}
