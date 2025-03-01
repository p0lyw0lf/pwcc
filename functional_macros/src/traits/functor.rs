use std::collections::HashSet;
use std::ops::Deref;

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;
use syn::punctuated::Punctuated;
use syn::Token;

use crate::emitter::make_fn_body;
use crate::emitter::make_variant_constructor;
use crate::emitter::BodyEmitter;
use crate::emitter::FieldEmitter;
use crate::generics::generics_add_suffix;
use crate::generics::generics_merge;
use crate::generics::instantiation_add_suffix;
use crate::generics::Behavior;
use crate::nodes::instantiation_collect_context;
use crate::nodes::ANode;
use crate::nodes::ANodes;
use crate::nodes::AType;
use crate::nodes::AVariant;

pub trait BaseCaseEmitter {
    fn base_case(
        &self,
        impl_generics: impl ToTokens,
        where_clause: impl ToTokens,
        input: impl ToTokens,
        output: impl ToTokens,
    ) -> TokenStream2;
}

/// Emits the impl Functor<T> for T implementation for the given type, applying generic arguments
/// as applicable
fn emit_base_case<'ast>(
    out: &mut TokenStream2,
    node: &ANode<'ast>,
    emitter: &impl BaseCaseEmitter,
) {
    let ident = node.ident();

    let generics = node.generics();
    let ctx = node.ctx();

    // If this is a recursive type, we should _not_ generate a base case, and instead let the
    // inductive case take care of that.
    if node.all_tys().any(|ty| ty.ident == ident) {
        return;
    }

    let input_generics = generics_add_suffix(generics, "Input", &ctx, Behavior::KeepAll);
    let output_generics = generics_add_suffix(generics, "Output", &ctx, Behavior::KeepAll);

    let all_generics = generics_merge(&input_generics, &output_generics);

    let (impl_generics, _, where_clause) = all_generics.split_for_impl();
    let (_, input_ty_generics, _) = input_generics.split_for_impl();
    let (_, output_ty_generics, _) = output_generics.split_for_impl();

    let out_toks = emitter.base_case(
        impl_generics,
        where_clause,
        quote! { #ident #input_ty_generics },
        quote! { #ident #output_ty_generics },
    );

    // eprintln!("{out_toks}");
    out.append_all(out_toks);
}

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
            impl #impl_generics Functor<#output> for #input #where_clause {
                type Input = #input;
                type Mapped = #output;
                fn fmap(self, f: &mut impl FnMut(Self::Input) -> #output) -> Self::Mapped {
                    f(self)
                }
            }
        }
    }
}

pub trait InductiveCaseEmitter {
    fn inductive_case<'ast>(
        &self,
        container: &ANode<'ast>,
        inner: &AType<'ast>,
        impl_generics: impl ToTokens,
        where_clause: impl ToTokens,
        input_outer: impl ToTokens,
        input_inner: impl ToTokens,
        output_outer: impl ToTokens,
        output_inner: impl ToTokens,
    ) -> TokenStream2;
}

/// Emit the impl Functor<Inner> for Container implementation for the given inner type and
/// container, applying all generic arguments as applicable.
fn emit_inductive_case<'ast>(
    out: &mut TokenStream2,
    lattice: &ANodes<'ast>,
    container: &ANode<'ast>,
    inner: &AType<'ast>,
    emitter: &impl InductiveCaseEmitter,
) {
    let ident = container.ident();
    let inner_node = lattice.get(inner.ident).expect("missing node");
    let inner_ident = inner_node.ident();

    let container_ctx = container.ctx();
    let inner_ctx =
        instantiation_collect_context(container_ctx, inner.instantiation.iter().map(Deref::deref));

    let container_generics = container.generics();

    let container_input_generics_partial =
        generics_add_suffix(container_generics, "Input", &inner_ctx, Behavior::OnlyCtx);
    let container_input_generics_full =
        generics_add_suffix(container_generics, "Input", &inner_ctx, Behavior::KeepAll);
    let container_output_generics =
        generics_add_suffix(container_generics, "Output", &inner_ctx, Behavior::KeepAll);

    let all_generics = generics_merge(
        &container_input_generics_partial,
        &container_output_generics,
    );

    let (impl_generics, _, where_clause) = all_generics.split_for_impl();
    let (_, input_ty_generics, _) = container_input_generics_full.split_for_impl();
    let (_, output_ty_generics, _) = container_output_generics.split_for_impl();

    let inner_instantiation = inner.instantiation.iter().map(Deref::deref);
    let input_inner_instantiation =
        instantiation_add_suffix(inner_instantiation.clone(), "Input", &inner_ctx)
            .collect::<Punctuated<_, Token![,]>>();

    let output_inner_instantiation =
        instantiation_add_suffix(inner_instantiation, "Output", &inner_ctx)
            .collect::<Punctuated<_, Token![,]>>();

    let out_toks = emitter.inductive_case(
        container,
        inner,
        impl_generics,
        where_clause,
        quote! { #ident #input_ty_generics },
        quote! { #inner_ident < #input_inner_instantiation > },
        quote! { #ident #output_ty_generics },
        quote! { #inner_ident < #output_inner_instantiation > },
    );

    // eprintln!("{out_toks}");
    out.append_all(out_toks);
}

impl InductiveCaseEmitter for Emitter {
    fn inductive_case<'ast>(
        &self,
        container: &ANode<'ast>,
        inner: &AType<'ast>,
        impl_generics: impl ToTokens,
        where_clause: impl ToTokens,
        input_outer: impl ToTokens,
        input_inner: impl ToTokens,
        output_outer: impl ToTokens,
        output_inner: impl ToTokens,
    ) -> TokenStream2 {
        let mut fn_body = make_fn_body(container, inner, output_inner.to_token_stream(), self);
        if container.ident() == inner.ident {
            fn_body = quote! {
                let out = { #fn_body };
                f(out)
            };
        }

        quote! {
            impl #impl_generics Functor<#output_inner> for #input_outer #where_clause {
                type Input = #input_inner;
                type Mapped = #output_outer;
                fn fmap(self, f: &mut impl FnMut(Self::Input) -> #output_inner) -> Self::Mapped {
                    #fn_body
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
        // This logic could be simplified further, but for the sake of clarity, I'd rather keep it in
        // separate cases like this
        if named {
            if has_inner {
                quote! { #ident: Functor::<#output_inner>::fmap(#ident, f) }
            } else {
                quote! { #ident }
            }
        } else {
            if has_inner {
                quote! { Functor::<#output_inner>::fmap(#ident, f) }
            } else {
                quote! { #ident }
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
        if in_enum {
            let ident = &variant.ident;
            make_variant_constructor(
                quote! { Self::Mapped::#ident },
                variant,
                inner,
                output_inner,
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
                output_inner,
                self,
            )
        }
    }
}

/// Emits all the code we need to generate into a TokenStream representing the interior of the
/// module.
pub fn emit<'ast>(
    out: &mut TokenStream2,
    nodes: &ANodes<'ast>,
    emitter: &(impl BaseCaseEmitter + InductiveCaseEmitter),
) {
    // Emit all base cases
    for node in nodes.values() {
        emit_base_case(out, node, emitter);
    }

    // Emit all inductive cases
    for container in nodes.values() {
        let types = container.all_tys().collect::<HashSet<_>>();
        for inner in types.into_iter() {
            emit_inductive_case(out, nodes, container, inner, emitter)
        }
    }
}
