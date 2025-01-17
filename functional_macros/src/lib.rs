use std::collections::HashSet;
use std::error::Error;
use std::fmt::Display;
use std::str::FromStr;

use proc_macro::TokenStream;
use proc_macro::TokenTree;

use quote::quote;
use quote::TokenStreamExt;
use syn::ItemMod;
use proc_macro2::TokenStream as TokenStream2;

mod emitter;
mod generics;
mod nodes;
mod syntax;
mod traits;

/// All the typeclasses we support
#[derive(PartialEq, Eq, Hash)]
enum Typeclass {
    #[cfg(feature = "foldable")]
    Foldable,
    #[cfg(feature = "functor")]
    Functor,
    #[cfg(feature = "try_functor")]
    TryFunctor,
}

#[derive(Debug)]
enum TypeclassParseError {
    BadName(String),
}

impl Display for TypeclassParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeclassParseError::BadName(name) => write!(f, "unexpected typeclass: {name}"),
        }
    }
}

impl Error for TypeclassParseError {}

impl FromStr for Typeclass {
    type Err = TypeclassParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            #[cfg(feature = "foldable")]
            "Foldable" => Ok(Typeclass::Foldable),
            #[cfg(feature = "functor")]
            "Functor" => Ok(Typeclass::Functor),
            #[cfg(feature = "try_functor")]
            "TryFunctor" => Ok(Typeclass::TryFunctor),
            other => Err(TypeclassParseError::BadName(other.into())),
        }
    }
}

/// A parsed set of typeclasses we should generate. If no typeclasses are specified, we should
/// enable all the ones we have features for.
struct EnabledTypeclasses(Option<HashSet<Typeclass>>);

/// Turns a ParseResult<T> into a T, breaking from the loop if None, and panicking if Some(Err)
macro_rules! break_none {
    ($e:expr) => {
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

impl EnabledTypeclasses {
    fn parse_attrs(attrs: TokenStream) -> Self {
        let mut hs = HashSet::new();

        for_each_ident(&mut attrs.into_iter(), &mut |ident| {
            let typeclass = Typeclass::from_str(&ident.to_string()).expect("parsing typeclass");
            hs.insert(typeclass);
        });

        if hs.len() > 0 {
            Self(Some(hs))
        } else {
            Self(None)
        }
    }

    fn has_typeclass(&self, t: &Typeclass) -> bool {
        match &self.0 {
            None => true,
            Some(hs) => hs.contains(&t),
        }
    }
}

/// Turns a syntax::ParseResult<T> into a T, panicking if it's a None or Some(Err)
macro_rules! always {
    ($e:expr) => {{
        match $e {
            Some(Ok(t)) => t,
            Some(Err(e)) => panic!("{e}"),
            None => panic!("unexpected end of stream"),
        }
    }};
}

/// Turns a syntax::ParseResult<T> into a Result<T, ParseError>, panicking if its None
macro_rules! maybe {
    ($e:expr) => {
        match $e {
            Some(v) => v,
            None => panic!("unexpected end of stream"),
        }
    };
}

#[proc_macro_attribute]
pub fn ast(attrs: TokenStream, item: TokenStream) -> TokenStream {
let crate_name = proc_macro2::Ident::new("functional", proc_macro2::Span::call_site());

    let enabled = EnabledTypeclasses::parse_attrs(attrs);

    let r#mod: ItemMod = syn::parse(item.clone()).expect("must be applied to module");

    let nodes = crate::nodes::make_nodes(&r#mod);

    let mut out = TokenStream::new();
    let iter = &mut item.into_iter();

    // All parse errors should have been handled earlier by syn, just do these assertions for extra safety
    loop {
        let res = maybe!(syntax::outer_attribute(&mut out, iter));
        if res.is_err() {
            break;
        }
    }
    let _ = maybe!(syntax::visibility(&mut out, iter));
    always!(syntax::keyword(&mut out, iter, "mod"));
    let _ = always!(syntax::ident(&mut out, iter));
    let group = always!(syntax::group(iter, proc_macro::Delimiter::Brace));

    // The reason we're doing this is to be able to output directly into the inner part of the
    // module, without having to round-trip through syn first.
    let group = {
        let mut out: TokenStream2 = group.stream().into();

        // First: all traits that do not care about coherence
        let nodes = crate::nodes::lattice::make_lattice(nodes);

        #[cfg(feature = "foldable")]
        if enabled.has_typeclass(&Typeclass::Foldable) {
            out.append_all(quote! {
                use #crate_name::Foldable;
            });
            traits::foldable::emit(&mut out, &nodes);
        }

        // Next: all traits that _do_ care about coherence
        let nodes = crate::nodes::coherence::filter_coherent(nodes);

        #[cfg(feature = "functor")]
        if enabled.has_typeclass(&Typeclass::Functor) || enabled.has_typeclass(&Typeclass::TryFunctor) {
            out.append_all(quote! {
                use #crate_name::Functor;
            });
            traits::functor::emit(&mut out, &nodes, &traits::functor::Emitter);
        }

        #[cfg(feature = "try_functor")]
        if enabled.has_typeclass(&Typeclass::TryFunctor) {
            out.append_all(quote! {
                use #crate_name::ControlFlow;
                use #crate_name::Semigroup;
                use #crate_name::TryFunctor;
            });
            traits::functor::emit(&mut out, &nodes, &traits::try_functor::Emitter);
        }

        proc_macro::Group::new(proc_macro::Delimiter::Brace, out.into())
    };
    out.extend([proc_macro::TokenTree::from(group)]);

    // There shouldn't be anything left in the iterator
    if let Some(remaining) = iter.next() {
        panic!("unexpected continuation of stream after module: {remaining}");
    }

    out
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
    let _ = always!(syntax::punct(
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
