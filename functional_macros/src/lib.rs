use std::collections::HashSet;
use std::error::Error;
use std::fmt::Display;
use std::str::FromStr;

use proc_macro::TokenStream;
use proc_macro::TokenTree;

use syn::ItemMod;

#[cfg(feature = "functor")]
mod functor;
mod nodes;
mod syntax;

/// All the typeclasses we support
#[derive(PartialEq, Eq, Hash)]
enum Typeclass {
    #[cfg(feature = "functor")]
    Functor,
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
            #[cfg(feature = "functor")]
            "Functor" => Ok(Typeclass::Functor),
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

    #[cfg(feature = "functor")]
    fn functor(&self) -> bool {
        match &self.0 {
            None => true,
            Some(hs) => hs.contains(&Typeclass::Functor),
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
        let mut out = group.stream().into();

        #[cfg(feature = "functor")]
        if enabled.functor() {
            functor::emit(&mut out, &nodes);
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
/// ```rust
/// #[specialize(T -> A, B, C)]
/// const FOO_T = BAR_T + BAZ_T;
///
/// #[specialize(_T_ -> Foo, Bar, Baz)]
/// impl Display for _T_ {
///     fn fmt(&self, f: Formatter<'_>) -> fmt::Result {
///         write!(f, "{}", self.0)
///     }
/// }
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
