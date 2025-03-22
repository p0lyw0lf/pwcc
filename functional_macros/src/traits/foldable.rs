use std::collections::HashSet;

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;

use crate::emitter::BodyEmitter;
use crate::nodes::lattice::Lattice;
use crate::nodes::ANode;
use crate::nodes::AType;
use crate::nodes::AVariant;

fn emit_base_case<'ast>(out: &mut TokenStream2, node: &ANode<'ast>) {
    let ident = node.ident();
    // If this is a recursive type, we should _not_ generate a base case, and instead let the
    // inductive case take care of that.
    if node.is_included() && node.all_tys().any(|ty| ty.ident == ident) {
        return;
    }

    let generics = node.generics();
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let container = quote! { #ident #ty_generics };

    let out_toks = quote! {
        impl #impl_generics Foldable<#container> for #container #where_clause {
            // Use long names for the generics so that we have a low chance of overlap with
            // anything used inside container
            fn foldl_impl<'functional_macros, FunctionalMacros>(&'functional_macros self, f: &mut impl FnMut(FunctionalMacros, &'functional_macros #container) -> FunctionalMacros, acc: FunctionalMacros, _how: RecursiveCall) -> FunctionalMacros
            where
                #container: 'functional_macros
            {
                f(acc, self)
            }
        }
    };
    // eprintln!("{out_toks}");
    out.append_all(out_toks);
}

struct Emitter;

impl<'ast> BodyEmitter<'ast> for Emitter {
    type Context = (&'ast AType<'ast>, &'ast TokenStream2);
    fn emit_variant_body(
        &self,
        variant: &AVariant<'ast>,
        (inner, output_inner): &Self::Context,
        _in_enum: bool,
    ) -> TokenStream2 {
        let output_inner = output_inner.into_token_stream();
        let folds = variant.fields.iter().filter_map(|field| {
            let has_inner = field.all_tys().any(|ty| ty == *inner);
            if !has_inner {
                return None;
            }
            let ident = &field.ident;
            Some(quote! {
                let acc = Foldable::<#output_inner>::foldl_impl(#ident, &mut f, acc, how);
            })
        });

        quote! {
            #(#folds)*
            acc
        }
    }
}

fn emit_inductive_case<'ast>(out: &mut TokenStream2, container: &ANode<'ast>, inner: &AType<'ast>) {
    let ident = container.ident();

    let container_generics = container.generics();
    let inner_instantiation = &inner.instantiation;

    let (impl_generics, ty_generics, where_clause) = container_generics.split_for_impl();
    let inner_ty_generics = quote! { < #(#inner_instantiation),* > };

    let output_container = quote! { #ident #ty_generics };
    let inner_ident = &inner.ident;
    let output_inner = quote! { #inner_ident #inner_ty_generics };

    let mut fn_body = Emitter.emit_body(quote! { self }, container, &(inner, &output_inner));
    if ident == inner.ident {
        fn_body = quote! {
            let body = |mut f, acc| -> FunctionalMacros { #fn_body };
            match how {
                RecursiveCall::Begin => {
                    let acc = f(acc, self);
                    let acc = body(f, acc);
                    acc
                }
                RecursiveCall::End => {
                    let acc = body(&mut f, acc);
                    let acc = f(acc, self);
                    acc
                }
                RecursiveCall::None => f(acc, self),
            }
        };
    }

    let out_toks = quote! {
        impl #impl_generics Foldable<#output_inner> for #output_container #where_clause {
            // Use long names for the generics so that we have a low chance of overlap with
            // anything used inside output_inner
            #[allow(unused_variables)]
            fn foldl_impl<'functional_macros, FunctionalMacros>(&'functional_macros self, mut f: &mut impl FnMut(FunctionalMacros, &'functional_macros #output_inner) -> FunctionalMacros, acc: FunctionalMacros, how: RecursiveCall) -> FunctionalMacros
            where
                #output_inner: 'functional_macros
            {
                #fn_body
            }
        }
    };
    // eprintln!("{out_toks}");
    out.append_all(out_toks);
}

pub fn emit<'ast>(out: &mut TokenStream2, nodes: &Lattice<'ast>) {
    for container in nodes.values() {
        if !container.emittable() {
            continue;
        }

        // Always emit base cases
        emit_base_case(out, container);

        let types = container.all_tys().collect::<HashSet<_>>();
        for inner in types.into_iter() {
            if nodes
                .0
                .get(&inner.ident)
                // Only emit inductive cases for included nodes
                .is_some_and(|node| node.is_included())
            {
                emit_inductive_case(out, container, inner);
            }
        }
    }
}
