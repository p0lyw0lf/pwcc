use std::collections::HashSet;

use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::Generics;
use syn::Ident;
use syn::Token;

#[derive(Default)]
pub struct Options {
    pub extra_nodes: Vec<ExtraNode>,
    enabled: Option<EnabledTypeclasses>,
}

impl Options {
    pub fn has_typeclass(&self, t: &Typeclass) -> bool {
        match self.enabled.as_ref() {
            Some(e) => e.0.contains(t),
            None => true,
        }
    }
}

/// All the typeclasses we support
#[derive(PartialEq, Eq, Hash)]
pub enum Typeclass {
    #[cfg(feature = "foldable")]
    Foldable,
    #[cfg(feature = "functor")]
    Functor,
    #[cfg(feature = "try_functor")]
    TryFunctor,
}

impl TryFrom<Ident> for Typeclass {
    type Error = syn::parse::Error;
    fn try_from(value: Ident) -> Result<Self, Self::Error> {
        match value.to_string().as_str() {
            #[cfg(feature = "foldable")]
            "Foldable" => Ok(Typeclass::Foldable),
            #[cfg(feature = "functor")]
            "Functor" => Ok(Typeclass::Functor),
            #[cfg(feature = "try_functor")]
            "TryFunctor" => Ok(Typeclass::TryFunctor),
            other => Err(syn::parse::Error::new(
                value.span(),
                format!("unexpected typeclass \"{}\"", other),
            )),
        }
    }
}

/// A parsed set of typeclasses we should generate. If no typeclasses are specified, we should
/// enable all the ones we have features for.
struct EnabledTypeclasses(HashSet<Typeclass>);

impl Parse for EnabledTypeclasses {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let idents = Punctuated::<Ident, Token![,]>::parse_terminated(input)?;
        let typeclasses = idents
            .into_iter()
            .map(Typeclass::try_from)
            .collect::<syn::Result<HashSet<_>>>()?;
        Ok(Self(typeclasses))
    }
}

/// An extra node we should add to the module that is considered part of the tree.
#[derive(Debug)]
pub struct ExtraNode {
    pub ident: Ident,
    pub generics: Generics,
}

impl Parse for ExtraNode {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            ident: input.parse()?,
            generics: input.parse()?,
        })
    }
}

impl Parse for Options {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let all_options =
            Punctuated::<Options, Token![,]>::parse_terminated_with(input, |input| {
                let ident = Ident::parse(input)?;
                match ident.to_string().as_str() {
                    "typeclasses" => {
                        let _ = <Token![=]>::parse(input)?;
                        let enabled;
                        let _ = syn::bracketed!(enabled in input);
                        Ok(Options {
                            extra_nodes: Vec::new(),
                            enabled: Some(enabled.parse()?),
                        })
                    }
                    "extra_nodes" => {
                        let _ = <Token![=]>::parse(input)?;
                        let extra_nodes;
                        let _ = syn::bracketed!(extra_nodes in input);
                        Ok(Options {
                            extra_nodes: Punctuated::<ExtraNode, Token![,]>::parse_terminated(
                                &extra_nodes,
                            )?
                            .into_iter()
                            .collect(),
                            enabled: None,
                        })
                    }
                    _ => todo!(),
                }
            })?;

        let out = all_options
            .into_iter()
            .fold(Options::default(), |mut left, right| Options {
                extra_nodes: {
                    left.extra_nodes.extend(right.extra_nodes);
                    left.extra_nodes
                },
                enabled: match (left.enabled, right.enabled) {
                    (Some(mut left), Some(right)) => {
                        left.0.extend(right.0);
                        Some(left)
                    }
                    (Some(left), None) => Some(left),
                    (None, Some(right)) => Some(right),
                    (None, None) => None,
                },
            });

        Ok(out)
    }
}
