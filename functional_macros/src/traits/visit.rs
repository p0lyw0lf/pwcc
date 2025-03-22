use proc_macro2::Span as Span2;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::TokenStreamExt;
use syn::Ident;

use crate::emitter::BodyEmitter;
use crate::nodes::lattice::Lattice;
use crate::nodes::AVariant;

/// Emits a module `prefix` (visit), with functions `prefix_*` (visit_*) for a trait `trait_name`
/// (Visit), for reference types `ref_ty` (&'ast), using `associated_method` (foldl_impl) to map
/// inside fields.
pub(super) struct Emitter<'a> {
    pub prefix: &'a str,
    pub trait_name: Ident,
    pub ref_ty: TokenStream2,
    pub associated_method: Ident,
}

impl<'a> Emitter<'a> {
    /// Converts a type name like "TypeName" into something like "visit_type_name"
    fn to_method(&self, ty: &Ident) -> Ident {
        let type_name = ty.to_string();
        let mut out = String::with_capacity(self.prefix.len() + 1 + type_name.len());
        out.push_str(self.prefix);
        let mut last_end = 0;
        for (start, c) in type_name.match_indices(|c| char::is_ascii_uppercase(&c)) {
            out.push_str(unsafe { type_name.get_unchecked(last_end..start) });
            out.push('_');
            out.push_str(&c.to_ascii_lowercase());
            last_end = start + c.len();
        }
        out.push_str(unsafe { type_name.get_unchecked(last_end..type_name.len()) });
        Ident::new(&out, Span2::call_site())
    }

    /// Given the set of nodes, emits a definition for the Visit trait.
    fn emit_trait<'ast>(&self, nodes: &Lattice<'ast>) -> TokenStream2 {
        let mut inner = TokenStream2::new();
        let ref_ty = &self.ref_ty;

        for node in nodes.values() {
            let ident = &node.ident();
            let method = self.to_method(ident);

            inner.append_all(quote! {
                fn #method(&mut self, node: #ref_ty #ident) {
                    #method(self, node)
                }
            });
        }

        let trait_name = &self.trait_name;
        quote! {
            pub trait #trait_name<'ast> {
                #inner
            }
        }
    }

    /// Given the set of nodes, emits all the `visit_*` functions needed to implement the Visit trait.
    fn emit_methods<'ast>(&self, nodes: &Lattice<'ast>) -> TokenStream2 {
        let mut out = TokenStream2::new();
        let trait_name = &self.trait_name;
        let ref_ty = &self.ref_ty;

        for node in nodes.values() {
            let ident = &node.ident();
            let method = self.to_method(ident);

            let body = self.emit_body(quote! { node }, node, &());

            out.append_all(quote! {
                #[allow(unused_variables)]
                pub fn #method<'ast, FunctionalMacros>(mut v: &mut FunctionalMacros, node: #ref_ty #ident)
                where
                    FunctionalMacros: #trait_name<'ast> + ?Sized,
                {
                    #body
                }
            });
        }

        out
    }

    pub fn emit<'ast>(&self, nodes: &Lattice<'ast>) -> TokenStream2 {
        let def_trait = self.emit_trait(nodes);
        let def_methods = self.emit_methods(nodes);

        quote! {
            pub mod visit {
                use super::*;
                #def_trait
                #def_methods
            }
        }
    }
}

impl<'ast> BodyEmitter<'ast> for Emitter<'_> {
    type Context = ();
    fn emit_variant_body(&self, variant: &AVariant<'ast>, _: &(), _in_enum: bool) -> TokenStream2 {
        let mut out = TokenStream2::new();
        let associated_method = &self.associated_method;

        for field in variant.fields.iter() {
            let field_ident = &field.ident;
            for ty in field.tys.iter() {
                let method = self.to_method(&ty.ident);
                // It's a little strange, but it works! We do need something to go into wrapper
                // types like Box and Option, so this is that.
                out.append_all(quote! {
                    v = #field_ident.#associated_method(&mut |v: &mut FunctionalMacros, n| { v.#method(n); v }, v, RecursiveCall::None);
                });
            }
        }

        out
    }
}

pub fn emit<'ast>(nodes: &Lattice<'ast>) -> TokenStream2 {
    Emitter {
        prefix: "visit",
        trait_name: Ident::new("Visit", Span2::call_site()),
        ref_ty: quote! { &'ast },
        associated_method: Ident::new("foldl_impl", Span2::call_site()),
    }
    .emit(nodes)
}
