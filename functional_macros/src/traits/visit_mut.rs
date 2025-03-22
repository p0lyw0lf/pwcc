use proc_macro2::Span as Span2;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::Ident;

use crate::nodes::lattice::Lattice;
use crate::traits::visit::Emitter;

pub fn emit<'ast>(nodes: &Lattice<'ast>) -> TokenStream2 {
    Emitter {
        prefix: "visit_mut",
        trait_name: Ident::new("VisitMut", Span2::call_site()),
        ref_ty: quote! { &'ast mut },
        associated_method: Ident::new("foldl_mut_impl", Span2::call_site()),
    }
    .emit(nodes)
}
