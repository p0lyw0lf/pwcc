use proc_macro::TokenStream;

use proc_macro2::Ident;
use proc_macro2::Span as Span2;

use quote::ToTokens;
use quote::quote;
use syn::ItemEnum;
use syn::ItemStruct;

#[proc_macro_derive(Spanned)]
pub fn spanned(item: TokenStream) -> TokenStream {
    let crate_name = Ident::new("pwcc_util", proc_macro2::Span::call_site());

    let item: syn::Item = syn::parse(item).expect("must be applied to item");

    match item {
        syn::Item::Struct(item) => spanned_struct(crate_name, item),
        syn::Item::Enum(item) => spanned_enum(crate_name, item),
        _ => panic!("can only be applied to struct or enum"),
    }
}

fn spanned_struct(crate_name: Ident, item: ItemStruct) -> TokenStream {
    let item_ident = &item.ident;
    let span_field_index = match item.fields.iter().position(is_span_field) {
        Some(field) => field,
        None => panic!("Did not find field type `Span` in struct {item_ident}"),
    };

    let span_field_ident = match item.fields {
        syn::Fields::Named(fields) => fields.named[span_field_index]
            .ident
            .as_ref()
            .unwrap()
            .to_token_stream(),
        syn::Fields::Unnamed(_) => span_field_index.to_token_stream(),
        syn::Fields::Unit => panic!("Cannot apply to unit struct {item_ident}"),
    };

    quote! {
        impl #crate_name::span::Spanned for #item_ident {
            fn span(&self) -> #crate_name::span::Span {
                self.#span_field_ident
            }
        }
    }
    .into_token_stream()
    .into()
}

fn spanned_enum(crate_name: Ident, item: ItemEnum) -> TokenStream {
    let item_ident = &item.ident;
    let cases = item.variants.iter().map(|variant| {
        let variant_ident = &variant.ident;
        let span_field_index = match variant.fields.iter().position(is_span_field) {
            Some(field) => field,
            None => match &variant.fields {
                syn::Fields::Named(_) => panic!(
                    "Did not find field type `Span` in enum variant {item_ident}::{variant_ident}"
                ),
                syn::Fields::Unit => panic!("Cannot apply to unit variant {item_ident}::{variant_ident}"),
                syn::Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                    return quote! { #item_ident::#variant_ident(n) => n.span(), };
                }
                syn::Fields::Unnamed(_) => panic!("Did not field type `Span` in enum variant {item_ident}::{variant_ident}. Must have exactly one field otherwise."),
            },
        };

        match &variant.fields {
            syn::Fields::Named(fields) => { let span_field = &fields.named[span_field_index]; 
                let span_field_ident = &span_field.ident;
                quote! { #item_ident::#variant_ident { #span_field_ident, .. } => *#span_field_ident, }
            }
            syn::Fields::Unnamed(fields) => {
                let num_fields = fields.unnamed.len();
                let mut current_index = 0usize;
                let matched_fields = std::iter::from_fn(move || {
                    if current_index >= num_fields {
                        return None;
                    }

                    let out = if current_index == span_field_index {
                        Ident::new("span", Span2::call_site())
                    } else { 
                        Ident::new("_", Span2::call_site())
                    };
                    current_index += 1;
                    Some(out)
                });

                quote! { #item_ident::#variant_ident(#(#matched_fields),*) => *span, }
            },
            syn::Fields::Unit => panic!("Cannot apply to unit variant {item_ident}::{variant_ident}"),
        }
    });

    quote! {
        impl #crate_name::span::Spanned for #item_ident {
            fn span(&self) -> #crate_name::span::Span {
                match self {
                    #(#cases)*
                }
            }
        }
    }
    .into_token_stream()
    .into()
}

fn is_span_field(field: &syn::Field) -> bool {
    match &field.ty {
        syn::Type::Path(ty_path) => {
            // I don't really want to have to resolve paths, so just check the last component
            // to make sure it is our special type name.
            ty_path
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "Span")
        }
        _ => false,
    }
}
