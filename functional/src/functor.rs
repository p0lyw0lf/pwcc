pub trait Functor<B> {
    type Input;
    type Output;
    type Mapped: Functor<B, Input=B, Mapped=Self::Mapped> + Functor<Self::Input, Input=B, Mapped=Self>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped;
}

impl<T, Inner, A, B> Functor<Inner> for Vec<T>
where
    T: Functor<Inner, Input=A, Output=B>,
{
    type Input = A;
    type Output = B;
    type Mapped = Vec<T::Mapped>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
        self.into_iter().map(&mut |x: T| x.fmap(f)).collect()
    }
}

impl<T, Inner, A, B> Functor<Inner> for Option<T>
where
    T: Functor<Inner, Input=A, Output=B>,
{
    type Input = A;
    type Output = B;
    type Mapped = Option<T::Mapped>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
        Option::map(self, &mut |x: T| x.fmap(f))
    }
}

#[macro_export]
macro_rules! functor {
    (type $base:ident <$input_type:ident : $input_trait:ident> -> <$output_type:ident : $output_trait:ident>) => {
        impl<$input_type: $input_trait, $output_type: $output_trait> $crate::functor::Functor<$base<$output_type>> for $base<$input_type> {
            type Input = $base<$input_type>;
            type Output = $base<$output_type>;
            type Mapped = $base<$output_type>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                f(self)
            }
        }
    };
    (struct $outer:ident <$input_type:ident : $input_trait:ident> -> <$output_type:ident : $output_trait:ident> where $input_inner:path |> $output_inner:path | {
        $($field:ident,)+
        ..,
        $($other_field:ident,)*
    }) => {
        impl<$input_type: $input_trait, $output_type: $output_trait> $crate::functor::Functor<$output_inner> for $outer<$input_type> {
            type Input = $input_inner;
            type Output = $output_inner;
            type Mapped = $outer<$output_type>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                $outer::<$output_type> {
                    $($field: $crate::functor::Functor::<$output_inner>::fmap(self.$field, f),)+
                    $($other_field: self.$other_field,)*
                }
            }
        }
    };
    (struct $outer:ident <$input_type:ident : $input_trait:ident> -> <$output_type:ident : $output_trait:ident> where $input_inner:path |> $output_inner:path | ($(
        $(+ $good_index:tt)?
        $(- $bad_index:tt)?
        ,
    )+)) => {
        impl<$input_type: $input_trait, $output_type: $output_trait> $crate::functor::Functor<$output_inner> for $outer<$input_type> {
            type Input = $input_inner;
            type Output = $output_inner;
            type Mapped = $outer<$output_type>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                $outer::<$output_type> ($(
                    $($crate::functor::Functor::<$output_inner>::fmap(self.$good_index, f),)?
                    $(self.$bad_index,)?
                )+)
            }
        }
    };
    (enum $outer:ident <$input_type:ident : $input_trait:ident> -> <$output_type:ident : $output_trait:ident> where $input_inner:path |> $output_inner:path | { $(
        $case:ident
        $({
            $($field:ident,)*
            ..,
            $($other_field:ident,)*
        })?
        $(($(
            $(+ $good_field:ident)?
            $(- $bad_field:ident)?
            ,
        )+))?
        ,
    )+ }) => {
        impl<$input_type: $input_trait, $output_type: $output_trait> $crate::functor::Functor<$output_inner> for $outer<$input_type> {
            type Input = $input_inner;
            type Output = $output_inner;
            type Mapped = $outer<$output_type>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                match self {
                $(
                    Self::$case
                    $({
                        $($field,)*
                        $($other_field,)*
                    })?
                    $(($(
                        $($good_field,)?
                        $($bad_field,)?
                    )+))?
                    => Self::Mapped::$case
                    $({
                        $($field: $crate::functor::Functor::<$output_inner>::fmap($field, f),)*
                        $($other_field,)*
                    })?
                    $(($(
                        $($crate::functor::Functor::<$output_inner>::fmap($good_field, f),)?
                        $($bad_field,)?
                    )+))?
                    ,
                )+
                }
            }
        }
    };
}
