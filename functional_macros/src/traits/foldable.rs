use std::collections::HashSet;

use proc_macro2::Span as Span2;
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use quote::TokenStreamExt;
use quote::quote;
use syn::Ident;

use crate::emitter::BodyEmitter;
use crate::nodes::ANode;
use crate::nodes::AType;
use crate::nodes::AVariant;
use crate::nodes::lattice::Lattice;

/// Emits a `trait_name` (Foldable) implementation for `method_name` (foldl_impl) for the given `ref_ty` (&'functional_macros)
pub(super) struct Emitter {
    pub trait_name: Ident,
    pub method_name: Ident,
    pub ref_ty: TokenStream2,
    pub has_lifetime: bool,
}

impl Emitter {
    fn method_generics(&self) -> TokenStream2 {
        if self.has_lifetime {
            quote! { <'functional_macros, FunctionalMacros> }
        } else {
            quote! { < FunctionalMacros > }
        }
    }
    fn method_where(&self, container: &TokenStream2) -> TokenStream2 {
        if self.has_lifetime {
            quote! {
                where
                    #container: 'functional_macros
            }
        } else {
            quote! {}
        }
    }

    fn emit_base_case<'ast>(&self, out: &mut TokenStream2, node: &ANode<'ast>) {
        let Emitter {
            trait_name,
            method_name,
            ref_ty,
            ..
        } = self;

        let ident = node.ident();
        // If this is a recursive type, we should _not_ generate a base case, and instead let the
        // inductive case take care of that.
        if node.is_included() && node.all_tys().any(|ty| ty.ident == ident) {
            return;
        }

        let generics = node.generics();
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let container = quote! { #ident #ty_generics };
        let method_generics = self.method_generics();
        let method_where = self.method_where(&container);

        let out_toks = quote! {
            impl #impl_generics #trait_name<#container> for #container #where_clause {
                // Use long names for the generics so that we have a low chance of overlap with
                // anything used inside container
                fn #method_name #method_generics(#ref_ty self, f: &mut impl FnMut(FunctionalMacros, #ref_ty #container) -> FunctionalMacros, acc: FunctionalMacros, _how: RecursiveCall) -> FunctionalMacros
                #method_where
                {
                    f(acc, self)
                }
            }
        };
        // eprintln!("{out_toks}");
        out.append_all(out_toks);
    }
}

impl<'ast> BodyEmitter<'ast> for Emitter {
    type Context = (&'ast AType<'ast>, &'ast TokenStream2);
    fn emit_variant_body(
        &self,
        variant: &AVariant<'ast>,
        (inner, output_inner): &Self::Context,
        _in_enum: bool,
    ) -> TokenStream2 {
        let Emitter {
            trait_name,
            method_name,
            ..
        } = self;
        let output_inner = output_inner.into_token_stream();
        let folds = variant.fields.iter().filter_map(|field| {
            let has_inner = field.all_tys().any(|ty| ty == *inner);
            if !has_inner {
                return None;
            }
            let ident = &field.ident;
            Some(quote! {
                let acc = #trait_name::<#output_inner>::#method_name(#ident, f, acc, how);
            })
        });

        quote! {
            #(#folds)*
            acc
        }
    }
}

impl Emitter {
    fn emit_inductive_case<'ast>(
        &self,
        out: &mut TokenStream2,
        container: &ANode<'ast>,
        inner: &AType<'ast>,
    ) {
        let Emitter {
            trait_name,
            method_name,
            ref_ty,
            ..
        } = self;

        let ident = container.ident();

        let container_generics = container.generics();
        let inner_instantiation = &inner.instantiation;

        let (impl_generics, ty_generics, where_clause) = container_generics.split_for_impl();
        let inner_ty_generics = quote! { < #(#inner_instantiation),* > };

        let output_container = quote! { #ident #ty_generics };
        let inner_ident = &inner.ident;
        let output_inner = quote! { #inner_ident #inner_ty_generics };

        let mut fn_body = self.emit_body(quote! { self }, container, &(inner, &output_inner));
        if ident == inner.ident {
            fn_body = quote! {
                if how == RecursiveCall::None {
                    return f(acc, self);
                }
                if how == RecursiveCall::Begin {
                    acc = f(acc, self);
                }
                acc = { #fn_body };
                if how == RecursiveCall::End {
                    acc = f(acc, self);
                }
                acc
            };
        }

        let method_generics = self.method_generics();
        let method_where = self.method_where(&output_inner);
        let out_toks = quote! {
            impl #impl_generics #trait_name<#output_inner> for #output_container #where_clause {
                // Use long names for the generics so that we have a low chance of overlap with
                // anything used inside output_inner
                #[allow(unused_variables)]
                fn #method_name #method_generics(#ref_ty self, mut f: &mut impl FnMut(FunctionalMacros, #ref_ty #output_inner) -> FunctionalMacros, mut acc: FunctionalMacros, how: RecursiveCall) -> FunctionalMacros
                #method_where
                {
                    #fn_body
                }
            }
        };
        // eprintln!("{out_toks}");
        out.append_all(out_toks);
    }

    pub fn emit<'ast>(&self, out: &mut TokenStream2, nodes: &Lattice<'ast>) {
        for container in nodes.values() {
            if !container.emittable() {
                continue;
            }

            // Always emit base cases
            self.emit_base_case(out, container);

            let types = container.all_tys().collect::<HashSet<_>>();
            for inner in types.into_iter() {
                if nodes
                    .0
                    .get(&inner.ident)
                    // Only emit inductive cases for included nodes
                    .is_some_and(|node| node.is_included())
                {
                    self.emit_inductive_case(out, container, inner);
                }
            }
        }
    }
}

pub fn emit<'ast>(out: &mut TokenStream2, nodes: &Lattice<'ast>) {
    Emitter {
        trait_name: Ident::new("Foldable", Span2::call_site()),
        method_name: Ident::new("foldl_impl", Span2::call_site()),
        ref_ty: quote! { &'functional_macros },
        has_lifetime: true,
    }
    .emit(out, nodes);
}
