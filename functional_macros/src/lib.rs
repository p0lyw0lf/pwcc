use proc_macro::TokenStream;
use syntax::visibility;
use syntax::module;

mod syntax;
mod types;
mod errors;

#[proc_macro_attribute]
pub fn ast(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Must be a module type. Parse that manually
    let iter = &mut item.clone().into_iter();
    // Doesn't matter if we find visibility or not
    let _ = visibility(iter).expect("encountered end of stream parsing visilibity");

    let res = module(iter).expect("encountered end of stream parsing ast module");
    if let Err(e) = res {
        panic!("{e}");
    }

    item
}
