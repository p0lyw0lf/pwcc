use std::collections::HashSet;

use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::Ident;
use syn::Token;

#[derive(Default)]
pub struct Options {
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
                            enabled: Some(enabled.parse()?),
                        })
                    }
                    // TODO: is there a better syn::Result return for this?
                    otherwise => panic!("unknown option {otherwise}"),
                }
            })?;

        let out = all_options
            .into_iter()
            .fold(Options::default(), |left, right| Options {
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
