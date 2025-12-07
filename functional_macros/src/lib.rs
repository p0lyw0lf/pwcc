use proc_macro::TokenStream;
use proc_macro::TokenTree;

use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use quote::TokenStreamExt;
use quote::quote;
use syn::ItemMod;

mod emitter;
mod generics;
mod nodes;
mod options;
mod syntax;
mod traits;

#[proc_macro_attribute]
pub fn ast(attrs: TokenStream, item: TokenStream) -> TokenStream {
    let crate_name = proc_macro2::Ident::new("functional", proc_macro2::Span::call_site());

    let options: options::Options = syn::parse(attrs).expect("invalid attribute syntax");

    let item_mod: ItemMod = syn::parse(item).expect("must be applied to module");

    let nodes = crate::nodes::make_nodes(&item_mod, &options.extra_nodes);

    let mut out = TokenStream2::new();

    fn is_outer(attr: &&syn::Attribute) -> bool {
        match attr.style {
            syn::AttrStyle::Outer => true,
            syn::AttrStyle::Inner(_) => false,
        }
    }
    fn is_inner(attr: &&syn::Attribute) -> bool {
        !is_outer(attr)
    }

    out.append_all(item_mod.attrs.iter().filter(is_outer));
    item_mod.vis.to_tokens(&mut out);
    item_mod.unsafety.to_tokens(&mut out);
    item_mod.mod_token.to_tokens(&mut out);
    item_mod.ident.to_tokens(&mut out);

    let (brace, items) = item_mod
        .content
        .as_ref()
        .expect("Must be applied to module with braces");

    brace.surround(&mut out, |out| {
        out.append_all(item_mod.attrs.iter().filter(is_inner));
        items.iter().for_each(|item| match item {
            syn::Item::Struct(s) => {
                out.append_all(
                    s.attrs
                        .iter()
                        .filter(|attr| !nodes::is_special_attr(attr) && is_outer(attr)),
                );
                s.vis.to_tokens(out);
                s.struct_token.to_tokens(out);
                s.ident.to_tokens(out);
                s.generics.to_tokens(out);
                match &s.fields {
                    syn::Fields::Named(fields) => {
                        s.generics.where_clause.to_tokens(out);
                        fields.to_tokens(out);
                    }
                    syn::Fields::Unnamed(fields) => {
                        fields.to_tokens(out);
                        s.generics.where_clause.to_tokens(out);
                        s.semi_token.to_tokens(out);
                    }
                    syn::Fields::Unit => {
                        s.generics.where_clause.to_tokens(out);
                        s.semi_token.to_tokens(out);
                    }
                }
            }
            syn::Item::Enum(e) => {
                out.append_all(
                    e.attrs
                        .iter()
                        .filter(|attr| !nodes::is_special_attr(attr) && is_outer(attr)),
                );
                e.vis.to_tokens(out);
                e.enum_token.to_tokens(out);
                e.ident.to_tokens(out);
                e.generics.to_tokens(out);
                e.generics.where_clause.to_tokens(out);
                e.brace_token.surround(out, |out| {
                    e.variants.to_tokens(out);
                });
            }
            otherwise => otherwise.to_tokens(out),
        });

        // First: all traits that do not care about coherence
        let nodes = crate::nodes::lattice::make_lattice(nodes);

        if options.has_any_typeclass(&[
            options::Typeclass::Foldable,
            options::Typeclass::FoldableMut,
            options::Typeclass::Functor,
            options::Typeclass::TryFunctor,
            options::Typeclass::Visit,
            options::Typeclass::VisitMut,
        ]) {
            out.append_all(quote! {
                use #crate_name::RecursiveCall;
            });
        }

        if options.has_any_typeclass(&[
            options::Typeclass::TryFunctor,
            options::Typeclass::Visit,
            options::Typeclass::VisitMut,
        ]) {
            out.append_all(quote! {
                use #crate_name::Semigroup;
            });
        }

        #[cfg(feature = "foldable")]
        if options.has_any_typeclass(&[options::Typeclass::Foldable, options::Typeclass::Visit]) {
            out.append_all(quote! {
                use #crate_name::Foldable;
            });
            traits::foldable::emit(out, &nodes);
        }

        #[cfg(feature = "foldable-mut")]
        if options.has_any_typeclass(&[
            options::Typeclass::FoldableMut,
            options::Typeclass::VisitMut,
        ]) {
            out.append_all(quote! {
                use #crate_name::FoldableMut;
            });
            traits::foldable_mut::emit(out, &nodes);
        }

        #[cfg(feature = "visit")]
        if options.has_typeclass(&options::Typeclass::Visit) {
            out.append_all(traits::visit::emit(&nodes));
        }

        #[cfg(feature = "visit-mut")]
        if options.has_typeclass(&options::Typeclass::VisitMut) {
            out.append_all(traits::visit_mut::emit(&nodes));
        }

        // Next: all traits that _do_ care about coherence
        let nodes = crate::nodes::coherence::filter_coherent(nodes);

        #[cfg(feature = "functor")]
        if options.has_any_typeclass(&[options::Typeclass::Functor, options::Typeclass::TryFunctor])
        {
            out.append_all(quote! {
                use #crate_name::Functor;
            });
            traits::functor::emit(out, &nodes, &traits::functor::Emitter);
        }

        #[cfg(feature = "try-functor")]
        if options.has_typeclass(&options::Typeclass::TryFunctor) {
            out.append_all(quote! {
                use #crate_name::ControlFlow;
                use #crate_name::TryFunctor;
            });
            traits::functor::emit(out, &nodes, &traits::try_functor::Emitter);
        }
    });

    out.into()
}

/// Turns a syntax::ParseResult<T> into a T, panicking if it's a None or Some(Err)
macro_rules! always {
    ($e:expr_2021) => {{
        match $e {
            Some(Ok(t)) => t,
            Some(Err(e)) => panic!("{e}"),
            None => panic!("unexpected end of stream"),
        }
    }};
}

/// Turns a ParseResult<T> into a T, breaking from the loop if None, and panicking if Some(Err)
macro_rules! break_none {
    ($e:expr_2021) => {
        match $e {
            None => break,
            Some(Err(e)) => panic!("{e}"),
            Some(Ok(t)) => t,
        }
    };
}

/// Expects attrs to be formatted as a comma-separated list of idents. Calls `f` on each ident in
/// the list.
fn for_each_ident(
    iter: &mut proc_macro::token_stream::IntoIter,
    f: &mut impl FnMut(proc_macro::Ident),
) {
    loop {
        let ident = break_none!(syntax::ident(&mut TokenStream::new(), iter));

        f(ident);

        break_none!(syntax::punct(
            &mut TokenStream::new(),
            iter,
            &[proc_macro::Punct::new(',', proc_macro::Spacing::Alone)]
        ));
    }
}

/// Helper macro for manually writing Functor instances, avoiding coherence conflicts by manually
/// specifying what types it will apply to. All this macro does is replace all instances of the
/// given in idents with all of the idents it's passed as arguments. Usage:
///
/// ```rust,ignore
/// const BAR_A: usize = 5;
/// const BAR_B: usize = 6;
/// const BAZ_A: usize = 7;
/// const BAZ_B: usize = 8;
/// #[specialize(T -> A, B)]
/// const FOO_T: usize = BAR_T + BAZ_T;
///
/// assert_eq!(FOO_A, 11);
/// assert_eq!(FOO_B, 15);
///
/// #[specialize(T -> Foo, Bar, Baz)]
/// struct T(usize);
///
/// #[specialize(T -> Foo, Bar, Baz)]
/// impl std::fmt::Display for T {
///     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
///         write!(f, "{}", self.0)
///     }
/// }
///
/// assert_eq!(&Foo(5).to_string(), "5");
/// assert_eq!(&Bar(6).to_string(), "6");
/// assert_eq!(&Baz(7).to_string(), "7");
/// ```
#[proc_macro_attribute]
pub fn specialize(attrs: TokenStream, item: TokenStream) -> TokenStream {
    let iter = &mut attrs.into_iter();
    let magic = always!(syntax::ident(&mut TokenStream::new(), iter));
    let () = always!(syntax::punct(
        &mut TokenStream::new(),
        iter,
        &[
            proc_macro::Punct::new('-', proc_macro::Spacing::Joint),
            proc_macro::Punct::new('>', proc_macro::Spacing::Alone)
        ]
    ));
    let magic = &magic.to_string();

    let mut out = TokenStream::new();

    for_each_ident(iter, &mut |ident| {
        out.extend(item.clone().into_iter().map(|token| match token {
            TokenTree::Ident(i) if i.to_string().contains(magic) => {
                TokenTree::Ident(proc_macro::Ident::new(
                    &i.to_string().replace(magic, &ident.to_string()),
                    ident.span(),
                ))
            }
            otherwise => otherwise,
        }));
    });

    out
}
