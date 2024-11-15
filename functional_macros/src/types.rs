use proc_macro::Ident;
use proc_macro::TokenStream;

#[derive(Debug, Clone)]
pub struct Struct {
    pub ident: Ident,
    pub generic_params: Option<TokenStream>,
    pub where_clause: Option<TokenStream>,
}

#[derive(Debug)]
pub struct StructStruct {
    pub base: Struct,
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct StructField {
    pub ident: Ident,
    pub r#type: Ident,
    pub generic_params: Option<TokenStream>,
}

#[derive(Debug)]
pub struct TupleStruct {
    pub base: Struct,
    pub fields: Vec<TupleField>,
}

#[derive(Debug)]
pub struct TupleField {
    pub r#type: Ident,
    pub generic_params: Option<TokenStream>,
}
