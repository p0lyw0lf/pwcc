pub trait Functor<B> {
    type Input;
    type Output;
    type Mapped; // : Functor<B, Input=B, Mapped=Self::Mapped> + Functor<Self::Input, Input=B, Mapped=Self>;
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

impl<T, E, Inner, A, B> Functor<Inner> for Result<T, E>
where
    T: Functor<Inner, Input=A, Output=B>,
{
    type Input = A;
    type Output = B;
    type Mapped = Result<T::Mapped, E>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
        Result::map(self, &mut |x: T| x.fmap(f))
    }
}


impl<T, E, Inner, A, B> Functor<Result<Inner, E>> for Vec<Result<T, E>>
where
    T: Functor<Inner, Input=A, Output=B>,
{
    type Input = A;
    type Output = B;
    type Mapped = Result<Vec<T::Mapped>, E>;
    fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
        // Result::map(self, &mut |x: T| x.fmap(f))
        todo!()
    }
}

#[macro_export]
macro_rules! functor {
    (type $base:ident) => {
        impl $crate::functor::Functor<$base> for $base {
            type Input = $base;
            type Output = $base;
            type Mapped = $base;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                f(self)
            }
        }
        impl<E> $crate::functor::Functor<Result<$base, E>> for $base {
            type Input = $base;
            type Output = Result<$base, E>;
            type Mapped = Result<$base, E>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                f(self)
            }
        }
    };
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
    (struct $outer:ident try for $inner:ident | {
        $($field:ident,)+
        ..,
        $($other_field:ident,)*
    }) => {
        impl<E> $crate::Functor<Result<$inner, E>> for $outer {
            type Input = $inner;
            type Output = Result<$inner, E>;
            type Mapped = Result<$outer, E>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                Ok($outer {
                    $($field: $crate::functor::Functor::<Result<$inner, E>>::fmap(self.$field, f)?,)+
                    $($other_field: self.$other_field,)*
                })
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
    (struct $outer:ident try for $inner:ident | ($(
        $(+ $good_index:tt)?
        $(- $bad_index:tt)?
        ,
    )+)) => {
        impl<E> $crate::functor::Functor<Result<$inner, E>> for $outer {
            type Input = $inner;
            type Output = Result<$inner, E>;
            type Mapped = Result<$outer, E>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                Ok($outer ($(
                    $($crate::functor::Functor::<Result<$inner, E>>::fmap(self.$good_index, f)?,)?
                    $(self.$bad_index,)?
                )+))
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
    (enum $outer:ident try for $inner:ident | { $(
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
        impl<E> $crate::functor::Functor<Result<$inner, E>> for $outer {
            type Input = $inner;
            type Output = Result<$inner, E>;
            type Mapped = Result<$outer, E>;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                Ok(match self {
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
                        $($field: $crate::functor::Functor::<Result<$inner, E>>::fmap($field, f)?,)*
                        $($other_field,)*
                    })?
                    $(($(
                        $($crate::functor::Functor::<Result<$inner, E>>::fmap($good_field, f)?,)?
                        $($bad_field,)?
                    )+))?
                    ,
                )+
                })
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
