use std::collections::HashSet;

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;

use crate::emitter::make_fn_body;
use crate::emitter::BodyEmitter;
use crate::nodes::lattice::Lattice;
use crate::nodes::ANode;
use crate::nodes::AType;
use crate::nodes::AVariant;

fn emit_base_case<'ast>(out: &mut TokenStream2, node: &ANode<'ast>) {
    let ident = node.ident();
    // If this is a recursive type, we should _not_ generate a base case, and instead let the
    // inductive case take care of that.
    if node.all_tys().any(|ty| ty.ident == ident) {
        return;
    }

    let generics = node.generics();
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let container = quote! { #ident #ty_generics };

    let out_toks = quote! {
        impl #impl_generics Foldable<#container> for #container #where_clause {
            // Use long names for the generics so that we have a low chance of overlap with
            // anything used inside container
            fn foldl<'functional_macros, FunctionalMacros>(&'functional_macros self, f: fn(FunctionalMacros, &'functional_macros #container) -> FunctionalMacros, acc: FunctionalMacros) -> FunctionalMacros
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

impl BodyEmitter for Emitter {
    fn body<'ast>(
        &self,
        variant: &AVariant<'ast>,
        inner: &AType<'ast>,
        output_inner: impl ToTokens,
        _in_enum: bool,
    ) -> TokenStream2 {
        let output_inner = output_inner.into_token_stream();
        let folds = variant.fields.iter().filter_map(|field| {
            let has_inner = field.all_tys().any(|ty| ty == inner);
            if !has_inner {
                return None;
            }
            let ident = &field.ident;
            Some(quote! {
                let acc = Foldable::<#output_inner>::foldl(#ident, f, acc);
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

    let mut fn_body = make_fn_body(container, inner, &output_inner, &Emitter);
    if ident == inner.ident {
        fn_body = quote! {
            f({ #fn_body }, self)
        };
    }

    let out_toks = quote! {
        impl #impl_generics Foldable<#output_inner> for #output_container #where_clause {
            // Use long names for the generics so that we have a low chance of overlap with
            // anything used inside output_inner
            #[allow(unused_variables)]
            fn foldl<'functional_macros, FunctionalMacros>(&'functional_macros self, f: fn(FunctionalMacros, &'functional_macros #output_inner) -> FunctionalMacros, acc: FunctionalMacros) -> FunctionalMacros
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
    for node in nodes.values() {
        emit_base_case(out, node);
    }

    for container in nodes.values() {
        let types = container.all_tys().collect::<HashSet<_>>();
        for inner in types.into_iter() {
            emit_inductive_case(out, container, inner);
        }
    }
}
