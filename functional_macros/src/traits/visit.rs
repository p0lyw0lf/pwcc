use proc_macro2::Span as Span2;
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use quote::TokenStreamExt;
use quote::quote;
use syn::Ident;

use crate::emitter::BodyEmitter;
use crate::nodes::AVariant;
use crate::nodes::lattice::Lattice;

/// Emits a module `prefix` (visit), with functions `prefix_*` (visit_*) for a trait `trait_name`
/// (Visit), for reference types `ref_ty` (&'ast), using `associated_method` (foldl_impl) to map
/// inside fields.
pub(super) struct Emitter<'a> {
    pub prefix: &'a str,
    pub trait_name: &'a str,
    pub ref_ty: TokenStream2,
    pub associated_method: Ident,
    pub has_lifetime: bool,
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

    /// Converts a type name like "TypeName" into 3 identifiers needed for doing recursion:
    ///
    /// + "visit_type_name_pre"
    /// + "visit_type_name"
    /// + "visit_type_name_post"
    ///
    /// The middle one is the one that will actually run the recursion, and the other ones are the
    /// only ones that should be overridden by implementors (for composability).
    ///
    /// TODO: figure out if it's possible to "seal" the implementation of the middle, so users can
    /// _never_ override it.
    fn to_methods(&self, ty: &Ident) -> (Ident, Ident) {
        let prefix = self.to_method(ty);
        let make_ident =
            |suffix: &str| Ident::new(&format!("{prefix}_{suffix}"), Span2::call_site());
        (make_ident("pre"), make_ident("post"))
    }

    /// The base name of the trait that external types will implement.
    fn trait_name(&self) -> TokenStream2 {
        let trait_name = &Ident::new(self.trait_name, Span2::call_site());
        if self.has_lifetime {
            quote! { #trait_name<'ast> }
        } else {
            trait_name.to_token_stream()
        }
    }

    /// Used to add helper functions to the trait, without allowing implementors to override them.
    fn extension_trait_name(&self) -> TokenStream2 {
        let extension_trait_name =
            &Ident::new(&format!("{}Ext", self.trait_name), Span2::call_site());
        if self.has_lifetime {
            quote! { #extension_trait_name<'ast> }
        } else {
            extension_trait_name.to_token_stream()
        }
    }

    fn generics(&self, extra: impl ToTokens) -> TokenStream2 {
        if self.has_lifetime {
            quote! { <'ast, FunctionalMacros, #extra> }
        } else {
            quote! { <FunctionalMacros, #extra> }
        }
    }

    /// Given the set of nodes, emits a definition for the Visit trait.
    fn emit_trait<'ast>(&self, nodes: &Lattice<'ast>) -> TokenStream2 {
        let mut body = TokenStream2::new();
        let ref_ty = &self.ref_ty;

        for node in nodes.values() {
            let ident = &node.ident();
            let (pre, post) = self.to_methods(ident);

            body.append_all(quote! {
                fn #pre(&mut self, node: #ref_ty #ident) {}
                fn #post(&mut self, node: #ref_ty #ident) {}
            });
        }

        let trait_name = self.trait_name();
        quote! {
            pub trait #trait_name {
                #body
            }
        }
    }

    fn emit_extension<'ast>(&self, nodes: &Lattice<'ast>) -> TokenStream2 {
        let trait_name = self.trait_name();
        let extension_trait_name = self.extension_trait_name();
        let generics = self.generics(TokenStream2::new());

        let recur_impl = self.emit_extension_recur(nodes);
        let (chain_def, chain_impl) = self.emit_extension_chain(nodes);

        quote! {
            mod private {
                pub trait Sealed {}
            }

            pub trait #extension_trait_name: #trait_name + private::Sealed {
                #recur_impl
                #chain_def
            }

            impl #generics private::Sealed for FunctionalMacros where FunctionalMacros: #trait_name {}
            impl #generics #extension_trait_name for FunctionalMacros where FunctionalMacros: #trait_name {
                #chain_impl
            }
        }
    }

    /// Given the set of nodes, emits all the `visit_*` functions needed to implement the Visit trait.
    fn emit_extension_recur<'ast>(&self, nodes: &Lattice<'ast>) -> TokenStream2 {
        let mut out = TokenStream2::new();
        let extension_trait_name = self.extension_trait_name();
        let generics = self.generics(TokenStream2::new());
        let ref_ty = &self.ref_ty;

        for node in nodes.values() {
            let ident = &node.ident();
            let method = self.to_method(ident);
            let (pre, post) = self.to_methods(ident);

            let body = self.emit_body(quote! { node }, node, &());

            out.append_all(quote! {
                #[allow(unused_variables)]
                fn #method(&mut self, node: #ref_ty #ident) {
                    #[inline(always)]
                    fn inner #generics (mut v: &mut FunctionalMacros, node: #ref_ty #ident)
                    where
                        FunctionalMacros: #extension_trait_name + ?Sized
                    {
                        #body
                    }

                    self.#pre(node);
                    inner(self, node);
                    self.#post(node);
                }
            });
        }

        out
    }

    /// Given the set of nodes, emit a helper function that allows for chaining of different
    /// visitors.
    fn emit_extension_chain<'ast>(&self, nodes: &Lattice<'ast>) -> (TokenStream2, TokenStream2) {
        let trait_name = self.trait_name();
        let def = quote! {
            fn chain<FunctionalMacros2: #trait_name>(self, other: FunctionalMacros2) -> impl #trait_name + Into<(Self, FunctionalMacros2)>
            where
                Self: Sized;
        };

        let mut body = TokenStream2::new();
        let ref_ty = &self.ref_ty;

        for node in nodes.values() {
            let ident = &node.ident();
            let (pre, post) = self.to_methods(ident);
            body.append_all(quote! {
                fn #pre(&mut self, node: #ref_ty #ident) {
                    self.first.#pre(node);
                    self.second.#pre(node);
                }
                fn #post(&mut self, node: #ref_ty #ident) {
                    self.second.#post(node);
                    self.first.#post(node);
                }
            });
        }

        let r#impl = quote! {
            fn chain<FunctionalMacros2: #trait_name>(self, other: FunctionalMacros2) -> impl #trait_name + Into<(Self, FunctionalMacros2)>
            where
                Self: Sized,
            {
                struct FunctionalMacrosVisitChain<A, B> {
                    first: A,
                    second: B,
                }

                impl<'ast, FunctionalMacrosA, FunctionalMacrosB> #trait_name for FunctionalMacrosVisitChain<FunctionalMacrosA, FunctionalMacrosB>
                where
                    FunctionalMacrosA: #trait_name,
                    FunctionalMacrosB: #trait_name,
                {
                    #body
                }

                impl<FunctionalMacrosA, FunctionalMacrosB> Into<(FunctionalMacrosA, FunctionalMacrosB)> for FunctionalMacrosVisitChain<FunctionalMacrosA, FunctionalMacrosB> {
                    fn into(self) -> (FunctionalMacrosA, FunctionalMacrosB) {
                        (self.first, self.second)
                    }
                }

                FunctionalMacrosVisitChain {
                    first: self,
                    second: other,
                }
            }
        };

        (def, r#impl)
    }

    /// It's currently not super ergonomic to write some of these visitors if they end up being
    /// simple functions like `FnMut(Node) -> Result<Node, Error>`. We'd like to make it so that we
    /// can write these visitors without having to do the boilerplate of defining a new struct,
    /// handling error coalescing, etc. Fortunately it is still possible to do this in a macro!
    ///
    /// One note: we will still have to make it so that the functions are `FnMut(&Node) ->
    fn emit_builder<'ast>(&self, nodes: &Lattice<'ast>) -> TokenStream2 {
        let trait_name = self.trait_name();
        let builder_ty_name =
            &Ident::new(&format!("{}Builder", self.trait_name), Span2::call_site());
        let ref_ty = &self.ref_ty;
        let generics = self.generics(quote! { FunctionalMacrosFn });

        let mut body = TokenStream2::new();
        for node in nodes.values() {
            let ident = &node.ident();
            // TODO: also do post if we need. Right now though, we only need pre, so might as well
            // generate less code lol
            let (pre, _post) = self.to_methods(ident);

            body.append_all(quote! {
                pub fn #pre #generics (f: FunctionalMacrosFn) -> impl #trait_name + Into<Result<(), FunctionalMacros>>
                where
                    // TODO: use #crate_name instead of hard-coding "functional" here
                    FunctionalMacros: functional::Semigroup,
                    FunctionalMacrosFn: FnMut(#ref_ty #ident) -> Result<(), FunctionalMacros>,
                {
                    struct FunctionalMacrosVisitor<FunctionalMacrosFn, FunctionalMacrosInner> {
                        f: FunctionalMacrosFn,
                        inner: Result<(), FunctionalMacrosInner>,
                    }

                    impl<FunctionalMacrosFn, FunctionalMacrosInner> Into<Result<(), FunctionalMacrosInner>> for FunctionalMacrosVisitor<FunctionalMacrosFn, FunctionalMacrosInner> {
                        fn into(self) -> Result<(), FunctionalMacrosInner> {
                            self.inner
                        }
                    }

                    impl #generics #trait_name for FunctionalMacrosVisitor<FunctionalMacrosFn, FunctionalMacros>
                    where
                        FunctionalMacros: functional::Semigroup,
                        FunctionalMacrosFn: FnMut(#ref_ty #ident) -> Result<(), FunctionalMacros>,
                    {
                        fn #pre(&mut self, node: #ref_ty #ident) {
                            let out = (self.f)(node);
                            self.inner.sconcat(out);
                        }
                    }

                    FunctionalMacrosVisitor {
                        f,
                        inner: Ok(()),
                    }
                }
            });
        }

        quote! {
            pub struct #builder_ty_name;
            impl #builder_ty_name {
                #body
            }
        }
    }

    pub fn emit<'ast>(&self, nodes: &Lattice<'ast>) -> TokenStream2 {
        let prefix = Ident::new(self.prefix, Span2::call_site());
        let def_trait = self.emit_trait(nodes);
        let def_extension = self.emit_extension(nodes);
        let def_builder = self.emit_builder(nodes);

        quote! {
            pub mod #prefix {
                use super::*;
                #def_trait
                #def_extension
                #def_builder
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
                let method = self.to_method(ty.ident);
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
        trait_name: "Visit",
        ref_ty: quote! { &'ast },
        associated_method: Ident::new("foldl_impl", Span2::call_site()),
        has_lifetime: true,
    }
    .emit(nodes)
}
