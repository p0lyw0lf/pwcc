use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::ToTokens;

use crate::functor::make_fn_body;
use crate::functor::BaseCaseEmitter;
use crate::functor::FieldEmitter;
use crate::functor::InductiveCaseEmitter;
use crate::nodes::ANode;
use crate::nodes::AType;

pub struct Emitter;

impl BaseCaseEmitter for Emitter {
    fn base_case(
        &self,
        impl_generics: impl ToTokens,
        where_clause: impl ToTokens,
        input: impl ToTokens,
        output: impl ToTokens,
    ) -> TokenStream2 {
        quote! {
            impl #impl_generics TryFunctor<#output> for #input #where_clause {
                fn try_fmap<E: Semigroup + ControlFlow>(self, f: &mut impl FnMut(<Self as Functor<#output>>::Input) -> Result<<Self as Functor<#output>>::Output, E>) -> Result<<Self as Functor<#output>>::Mapped, E> {
                    f(self)
                }
            }
        }
    }
}

impl InductiveCaseEmitter for Emitter {
    fn inductive_case<'ast>(
        &self,
        container: &ANode<'ast>,
        inner: &AType<'ast>,
        impl_generics: impl ToTokens,
        where_clause: impl ToTokens,
        input_outer: impl ToTokens,
        _input_inner: impl ToTokens,
        _output_outer: impl ToTokens,
        output_inner: impl ToTokens,
    ) -> TokenStream2 {
        let fn_body = make_fn_body(container, inner, output_inner.to_token_stream(), self);

        quote! {
            impl #impl_generics TryFunctor<#output_inner> for #input_outer #where_clause {
                fn try_fmap<E: Semigroup + ControlFlow>(self, f: &mut impl FnMut(<Self as Functor<#output_inner>>::Input) -> Result<<Self as Functor<#output_inner>>::Output, E>) -> Result<<Self as Functor<#output_inner>>::Mapped, E> {
                    Ok({ #fn_body })
                }
            }
        }
    }
}

impl FieldEmitter for Emitter {
    fn field(
        &self,
        named: bool,
        has_inner: bool,
        output_inner: impl ToTokens,
        ident: impl ToTokens,
    ) -> TokenStream2 {
        // TODO: make it so that the TryFunctor impl makes a MaybeUninit for the struct as a whole,
        // then all the fields are filled out, and only at the end (once all errors have been
        // collected) do we assume_init() it. This will make it so we can have proper fancy control
        // flow things, instead of the boring always-early-return stuff we have now.
        if named {
            if has_inner {
                quote! { #ident: TryFunctor::<#output_inner>::try_fmap(#ident, f)? }
            } else {
                quote! { #ident }
            }
        } else {
            if has_inner {
                quote! { TryFunctor::<#output_inner>::try_fmap(#ident, f)? }
            } else {
                quote! { #ident }
            }
        }
    }
}
