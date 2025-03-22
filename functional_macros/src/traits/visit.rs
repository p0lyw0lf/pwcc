use proc_macro2::Span as Span2;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::TokenStreamExt;
use syn::Ident;

use crate::emitter::BodyEmitter;
use crate::nodes::ANodes;
use crate::nodes::AVariant;

/// Emits a module `prefix` (visit), with functions `prefix_*` (visit_*) for a trait `trait_name`
/// (Visit)
pub(super) struct Emitter<'a> {
    prefix: &'a str,
    trait_name: Ident,
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
        Ident::new(&out, Span2::call_site())
    }

    /// Given the set of nodes, emits a definition for the Visit trait.
    fn emit_trait<'ast>(&self, nodes: &ANodes<'ast>) -> TokenStream2 {
        let mut inner = TokenStream2::new();

        for node in nodes.values() {
            let ident = &node.ident();
            let method = self.to_method(ident);

            inner.append_all(quote! {
                fn #method(&mut self, node: &'ast #ident) {
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
    fn emit_methods<'ast>(&self, nodes: &ANodes<'ast>) -> TokenStream2 {
        let mut out = TokenStream2::new();

        for node in nodes.values() {
            let ident = &node.ident();
            let method = self.to_method(ident);

            let body = self.emit_body(quote! { node }, node, &());

            out.append_all(quote! {
                pub fn #method<'ast, V>(v: &mut V, node: &'ast #ident)
                where
                    V: Visit<'ast> + ?Sized,
                {
                    #body
                }
            });
        }

        out
    }

    pub fn emit<'ast>(&self, nodes: &ANodes<'ast>) -> TokenStream2 {
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

        for field in variant.fields.iter() {
            let field_ident = &field.ident;
            for ty in field.tys.iter() {
                let method = self.to_method(&ty.ident);
                // It's a little strange, but it works! We do need something to go into wrapper
                // types like Box and Option, so this is that.
                out.append_all(quote! {
                    #field_ident.foldl_impl(&mut |(), n| v.#method(n), (), RecursiveCall::None);
                });
            }
        }

        out
    }
}

pub fn emit<'ast>(nodes: &ANodes<'ast>) -> TokenStream2 {
    Emitter {
        prefix: "visit",
        trait_name: Ident::new("Visit", Span2::call_site()),
    }
    .emit(nodes)
}
