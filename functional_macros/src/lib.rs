use proc_macro::TokenStream;
use syntax::parse_visibility;
use syntax::parse_module;

mod syntax;
mod errors;

#[proc_macro_attribute]
pub fn ast(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Must be a module type. Parse that manually
    let iter = &mut item.clone().into_iter();
    // Doesn't matter if we find visibility or not
    let _ = parse_visibility(iter);

    let _ = parse_module(iter).expect("parsing ast module");

    item
}
