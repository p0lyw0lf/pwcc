use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::ToTokens;

use crate::emitter::make_fn_body;
use crate::emitter::make_variant_constructor;
use crate::emitter::BodyEmitter;
use crate::emitter::FieldEmitter;
use crate::nodes::ANode;
use crate::nodes::AType;
use crate::nodes::AVariant;
use crate::traits::functor::BaseCaseEmitter;
use crate::traits::functor::InductiveCaseEmitter;

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
                fn try_fmap<E: Semigroup + ControlFlow>(self, f: &mut impl FnMut(<Self as Functor<#output>>::Input) -> Result<#output, E>) -> Result<<Self as Functor<#output>>::Mapped, E> {
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
        let mut fn_body = make_fn_body(container, inner, output_inner.to_token_stream(), self);
        if container.ident() == inner.ident {
            fn_body = quote! {
                let out = { #fn_body };
                f(out?)
            };
        }

        quote! {
            impl #impl_generics TryFunctor<#output_inner> for #input_outer #where_clause {
                fn try_fmap<E: Semigroup + ControlFlow>(self, f: &mut impl FnMut(<Self as Functor<#output_inner>>::Input) -> Result<#output_inner, E>) -> Result<<Self as Functor<#output_inner>>::Mapped, E> {
                    let mut err = Option::<E>::None;
                    #fn_body
                }
            }
        }
    }
}

impl BodyEmitter for Emitter {
    fn body<'ast>(
        &self,
        variant: &AVariant<'ast>,
        inner: &AType<'ast>,
        output_inner: impl ToTokens,
        in_enum: bool,
    ) -> TokenStream2 {
        let output_inner = output_inner.to_token_stream();
        let transforms = variant.fields.iter().filter_map(|field| {
            let has_inner = field.all_tys().any(|ty| ty == inner);
            if !has_inner {
                return None;
            }
            let ident = &field.ident;
            Some(quote! {
                let #ident = match TryFunctor::<#output_inner>::try_fmap(#ident, f) {
                    Ok(v) => ::core::mem::MaybeUninit::new(v),
                    Err(e) => {
                        let new_err = err.sconcat(Some(e)).unwrap();
                        if !new_err.cont() { return Err(new_err); }
                        err = Some(new_err);
                        ::core::mem::MaybeUninit::uninit()
                    }
                }
            })
        });

        let out = if in_enum {
            let ident = &variant.ident;
            make_variant_constructor(
                quote! { Self::Mapped::#ident },
                variant,
                inner,
                &output_inner,
                self,
            )
        } else {
            make_variant_constructor(
                if variant.named {
                    quote! { Self::Mapped }
                } else {
                    // TODO: I am not sure why, but for some reason, unnamed structs don't like
                    // doing Self::Mapped(), and prefer their original name instead. Exactly why is
                    // beyond me, but this seems to work, so whatever.
                    variant.ident.to_token_stream()
                },
                variant,
                inner,
                &output_inner,
                self,
            )
        };

        quote! {
            #(#transforms ;)*
            match err {
                Some(e) => Err(e),
                None => Ok(#out)
            }
        }
    }
}

impl FieldEmitter for Emitter {
    fn field(
        &self,
        named: bool,
        has_inner: bool,
        _output_inner: impl ToTokens,
        ident: impl ToTokens,
    ) -> TokenStream2 {
        // All the logic is taken care of in BodyEmitter; just need to unwrap all the fields that
        // were mapped.
        if named {
            if has_inner {
                quote! {
                    // SAFETY: if we get here, we never encountered an error, so we must be init
                    #ident: unsafe { #ident.assume_init() }
                }
            } else {
                ident.into_token_stream()
            }
        } else {
            if has_inner {
                quote! {
                    // SAFETY: if we get here, we never encountered an error, so we must be init
                    unsafe { #ident.assume_init() }
                }
            } else {
                ident.into_token_stream()
            }
        }
    }
}
