use proc_macro2::Span as Span2;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::Ident;

use crate::nodes::lattice::Lattice;
use crate::traits::foldable::Emitter;

pub fn emit<'ast>(out: &mut TokenStream2, nodes: &Lattice<'ast>) {
    Emitter {
        trait_name: Ident::new("FoldableMut", Span2::call_site()),
        method_name: Ident::new("foldl_mut_impl", Span2::call_site()),
        ref_ty: quote! { &mut },
        has_lifetime: false,
    }
    .emit(out, nodes);
}
