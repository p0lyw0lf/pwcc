use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::TokenStreamExt;

use crate::emitter::make_fn_body;
use crate::emitter::BodyEmitter;
use crate::nodes::ANodes;
use crate::nodes::AVariant;

/// Emits a module `prefix` (visit), with functions `prefix_*` (visit_*) for a trait `trait_name`
/// (Visit)
pub(super) struct Emitter<'a> {
    prefix: &'a str,
    trait_name: &'a str,
}

impl<'a> Emitter<'a> {
    /// Converts a type name like "TypeName" into something like "visit_type_name"
    fn to_method(&self, type_name: &str) -> String {
        let mut out = String::with_capacity(self.prefix.len() + 1 + type_name.len());
        out.push_str(self.prefix);
        let mut last_end = 0;
        for (start, c) in type_name.match_indices(|c| char::is_ascii_uppercase(&c)) {
            out.push_str(unsafe { type_name.get_unchecked(last_end..start) });
            out.push('_');
            out.push_str(&c.to_ascii_lowercase());
            last_end = start + c.len();
        }
        out
    }

    /// Given the set of nodes, emits a definition for the Visit trait.
    fn emit_trait<'ast>(&self, nodes: &ANodes<'ast>) -> TokenStream2 {
        let mut inner = TokenStream2::new();

        for node in nodes.values() {
            let ident = &node.ident().to_string();
            let method = self.to_method(ident);

            inner.append_all(quote! {
                fn #method(&mut self, node: &'ast #ident) {
                    #method(self, node)
                }
            });
        }

        let trait_name = self.trait_name;
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
            let ident = &node.ident().to_string();
            let method = self.to_method(ident);

            let body = make_fn_body(self, node, &());

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
                #def_trait
                #def_methods
            }
        }
    }
}

impl<'ast> BodyEmitter<'ast> for Emitter<'_> {
    type Context = ();
    fn body(&self, variant: &AVariant<'ast>, _: &(), _in_enum: bool) -> TokenStream2 {
        let mut out = TokenStream2::new();

        for field in variant.fields.iter() {
            let field_ident = &field.ident;
            for ty in field.tys.iter() {
                let method = self.to_method(&ty.ident.to_string());
                // TODO: this will not work because it doesn't consider fields with multiple types,
                // or fields with wrapper types like Option or Box. To solve that, I kinda need
                // something like Functor, but I can't have that, since that's what Visit is for in
                // the first place? Going to have to puzzle on this to figure out how to solve it.
                out.append_all(quote! {
                    #method(v, #field_ident);
                });
            }
        }

        out
    }
}

pub fn emit<'ast>(nodes: &ANodes<'ast>) -> TokenStream2 {
    Emitter {
        prefix: "visit",
        trait_name: "Visit",
    }
    .emit(nodes)
}
