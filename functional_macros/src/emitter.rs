use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::ToTokens;

use crate::nodes::AField;
use crate::nodes::ANode;
use crate::nodes::AType;
use crate::nodes::AVariant;

pub trait FieldEmitter {
    fn field(
        &self,
        named: bool,
        has_inner: bool,
        output_inner: impl ToTokens,
        ident: impl ToTokens,
    ) -> TokenStream2;
}

/// For a given field, decides if it should be fmapped or not, and returns the appropriate
/// expression
fn make_field<'ast>(
    named: bool,
    field: &AField<'ast>,
    inner: &AType<'ast>,
    output_inner: impl ToTokens,
    emitter: &impl FieldEmitter,
) -> TokenStream2 {
    let has_inner = field.all_tys().any(|ty| ty == inner);
    let ident = &field.ident;

    emitter.field(named, has_inner, output_inner, ident)
}

/// Makes a list of transformed fields, with the appropriate delimiter type based on whether the
/// fields are named or unnamed
pub fn make_variant_constructor<'ast>(
    container: TokenStream2,
    variant: &AVariant<'ast>,
    inner: &AType<'ast>,
    output_inner: impl ToTokens,
    emitter: &impl FieldEmitter,
) -> TokenStream2 {
    if variant.unit {
        return container;
    }
    let output_inner = output_inner.into_token_stream();

    let fields = variant
        .fields
        .iter()
        .map(|field| make_field(variant.named, field, inner, &output_inner, emitter));
    if variant.named {
        quote! { #container { #(#fields),* } }
    } else {
        quote! { #container ( #(#fields),* ) }
    }
}

/// Makes a line like
///
/// ```rust,ignore
/// Ident { x, y, z }
/// ```
///
/// or
///
/// ```rust,ignore
/// Ident(tmp_0, tmp_1)
/// ```
///
/// or
///
/// ```rust,ignore
/// Ident
/// ```
///
/// depending on the variant of the passed in fields.
fn make_variant_destructor<'ast>(ident: TokenStream2, variant: &AVariant<'ast>) -> TokenStream2 {
    if variant.unit {
        return ident;
    }

    let fields = variant.fields.iter().map(|field| &field.ident);
    if variant.named {
        quote! { #ident { #(#fields),* } }
    } else {
        quote! { #ident ( #(#fields),* ) }
    }
}

pub trait BodyEmitter {
    fn body<'ast>(
        &self,
        variant: &AVariant<'ast>,
        inner: &AType<'ast>,
        output_inner: impl ToTokens,
        in_enum: bool,
    ) -> TokenStream2;
}

/// Writes the body of the trait implementation. Automatically destructures the fields and puts
/// their idents in scope for when the emitter runs.
pub fn make_fn_body<'ast>(
    container: &ANode<'ast>,
    inner: &AType<'ast>,
    output_inner: impl ToTokens,
    emitter: &impl BodyEmitter,
) -> TokenStream2 {
    match container {
        ANode::Struct(s) => {
            let destructor = make_variant_destructor(quote! { Self }, &s.data);
            let body = emitter.body(&s.data, inner, &output_inner, false);

            quote! {
                let #destructor = self;
                #body
            }
        }
        ANode::Enum(e) => {
            let output_inner = output_inner.into_token_stream();
            let variants = e.variants.iter().map(|variant| {
                let ident = &variant.ident;
                let destructor = make_variant_destructor(quote! { Self::#ident }, variant);
                let body = emitter.body(variant, inner, &output_inner, true);

                quote! {
                    #destructor => { #body }
                }
            });

            quote! {
                match self {
                    #(#variants),*
                }
            }
        }
        ANode::Extern(x) => panic!("Should not emit extra node {}", x.ident),
    }
}
