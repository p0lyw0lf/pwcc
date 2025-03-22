use std::collections::HashSet;

use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::Ident;
use syn::Token;

use crate::nodes::ExtraNode;

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
    pub fn has_any_typeclass(&self, ts: &[Typeclass]) -> bool {
        match self.enabled.as_ref() {
            Some(e) => ts.iter().any(|t| e.0.contains(t)),
            None => true,
        }
    }
}

/// All the typeclasses we support
#[derive(PartialEq, Eq, Hash)]
pub enum Typeclass {
    Foldable,
    Functor,
    TryFunctor,
    Visit,
    VisitMut,
}

impl TryFrom<Ident> for Typeclass {
    type Error = syn::parse::Error;
    fn try_from(value: Ident) -> Result<Self, Self::Error> {
        match value.to_string().as_str() {
            #[cfg(feature = "foldable")]
            "Foldable" => Ok(Typeclass::Foldable),
            #[cfg(feature = "functor")]
            "Functor" => Ok(Typeclass::Functor),
            #[cfg(feature = "try-functor")]
            "TryFunctor" => Ok(Typeclass::TryFunctor),
            #[cfg(feature = "visit")]
            "Visit" => Ok(Typeclass::Visit),
            #[cfg(feature = "visit-mut")]
            "VisitMut" => Ok(Typeclass::VisitMut),
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
