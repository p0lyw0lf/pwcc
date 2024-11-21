use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::TokenStreamExt;
use syn::spanned::Spanned;
use syn::visit_mut;
use syn::visit_mut::VisitMut;
use syn::ConstParam;
use syn::Field;
use syn::Generics;
use syn::Ident;
use syn::Lifetime;
use syn::Token;
use syn::TypeParam;
use syn::WhereClause;

use crate::nodes::AField;
use crate::nodes::AType;
use crate::nodes::AVariant;
use crate::nodes::ANode;
use crate::nodes::GenericContext;
use crate::nodes::ANodes;

/// Visitor that will append a suffix to all generic parameters in a set of idents
struct AddSuffix<'ast, 'suffix, 'hashset> {
    // Should be CamelCase
    suffix: &'suffix str,
    params: &'hashset GenericContext<'ast>,
}

impl<'ast, 'suffix, 'hashset> VisitMut for AddSuffix<'ast, 'suffix, 'hashset> {
    fn visit_lifetime_mut(&mut self, l: &mut Lifetime) {
        if self.params.has_lifetime(&l.ident) {
            l.ident = Ident::new(
                &format!("{}_{}", l.ident, self.suffix.to_lowercase()),
                l.ident.span(),
            );
        }
        visit_mut::visit_lifetime_mut(self, l);
    }

    // TODO: also need to make the type and constant suffix-ification work for
    // instantiated type parameters, not just declared ones
    fn visit_type_param_mut(&mut self, tp: &mut TypeParam) {
        if self.params.has_type(&tp.ident) {
            tp.ident = Ident::new(&format!("{}{}", tp.ident, self.suffix), tp.ident.span());
        }
        visit_mut::visit_type_param_mut(self, tp);
    }

    fn visit_const_param_mut(&mut self, cp: &mut ConstParam) {
        if self.params.has_const(&cp.ident) {
            cp.ident = Ident::new(
                &format!("{}_{}", cp.ident, self.suffix.to_uppercase()),
                cp.ident.span(),
            );
        }
        visit_mut::visit_const_param_mut(self, cp);
    }
}

/// AddSuffixs a string to the idents of all of the given generic arguments, returning the resulting generics
fn generics_add_suffix(generics: &Generics, suffix: &str) -> Generics {
    // TODO
    generics.clone()
}

/// Merges two sets of generic parameters together
fn generics_merge(a: &Generics, b: &Generics) -> Generics {
    let params = a
        .params
        .clone()
        .into_iter()
        .chain(b.params.clone().into_iter())
        .collect();
    let where_clause = match (a.where_clause.as_ref(), b.where_clause.as_ref()) {
        (None, None) => None,
        (Some(a_where), None) => Some(a_where.clone()),
        (None, Some(b_where)) => Some(b_where.clone()),
        (Some(a_where), Some(b_where)) => {
            let predicates = a_where
                .predicates
                .clone()
                .into_iter()
                .chain(b_where.predicates.clone().into_iter())
                .collect();
            Some(WhereClause {
                where_token: <Token![where]>::default(),
                predicates,
            })
        }
    };

    Generics {
        lt_token: Some(<Token![<]>::default()),
        params,
        gt_token: Some(<Token![>]>::default()),
        where_clause,
    }
}

/// Emits the impl Functor<T> for T implementation for the given type, applying generic arguments
/// as applicable
fn emit_base_case<'ast>(out: &mut TokenStream2, node: &ANode<'ast>) {
    /*
    let input_generics = generics_add_suffix(generics, "Input");
    let output_generics = generics_add_suffix(generics, "Output");

    let all_generics = generics_merge(&input_generics, &output_generics);

    let (impl_generics, _, where_clause) = all_generics.split_for_impl();
    let (_, input_ty_generics, _) = input_generics.split_for_impl();
    let (_, output_ty_generics, _) = output_generics.split_for_impl();

    out.append_all(quote! {
        impl #impl_generics Functor<#ident #output_ty_generics> for #ident #input_ty_generics #where_clause {
            type Input = #ident #input_ty_generics;
            type Output = #ident #output_ty_generics;
            type Mapped = #ident #output_ty_generics;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                f(self)
            }
        }
    })
*/
}

/// Emit the impl Functor<Inner> for Container implementation for the given inner type and
/// container, applying all generic arguments as applicable.
fn emit_inductive_case<'ast>(
    out: &mut TokenStream2,
    lattice: &ANodes<'ast>,
    container: &ANode<'ast>,
    inner: &AType<'ast>,
) {
    let ident = container.ident();
    let inner_ident = &inner.key;
    let generics = container.ctx();

    /*

    let input_generics = generics_add_suffix(generics, "Input");
    let output_generics = generics_add_suffix(generics, "Output");

    let input_inner_generics = generics_add_suffix(inner.generics(), "Input");
    let (_, input_inner_ty_generics, _) = input_inner_generics.split_for_impl();
    let input_inner = quote! { #inner_ident #input_inner_ty_generics };

    let output_inner_generics = generics_add_suffix(inner.generics(), "Output");
    let (_, output_inner_ty_generics, _) = output_inner_generics.split_for_impl();
    let output_inner = quote! { #inner_ident #output_inner_ty_generics };

    let all_generics = generics_merge(&input_generics, &output_generics);

    let (impl_generics, _, where_clause) = all_generics.split_for_impl();
    let (_, input_ty_generics, _) = input_generics.split_for_impl();
    let (_, output_ty_generics, _) = output_generics.split_for_impl();

    let fn_body = make_fn_body(lattice, container, inner, output_inner.clone());
*/

    // TODO: currently, this doesn't work for things like
    //
    // ```rust
    // struct LeftExp<T>(T);
    // struct RightExp<T>(T);
    // enum Either<A, B> {
    //     Left(LeftExp<A>),
    //     Right(RightExp<B>),
    // }
    // ```
    //
    // when going from `Either` to `LeftExp` or `RightExp`, because #all_generics is too broad.
    // Unfortunately, fixing this bug requires keeping track of how each child type uses each
    // generic, which seems pretty hard...
    //
    // This _also_ won't work for things like
    //
    // ```rust
    // struct Exp<T>(T);
    // enum Statement {
    //     Return(Exp<String>),
    // }
    // ```
    //
    // because now we accidentally transform the type parameter of `Exp` to
    // `StringInner`/`StringOuter`. Again, properly handling this requires keeping track of if/how
    // type parameters from the parent node are used in children fields, which is too much effort.
    // Best I can do is make it work only when the entire tree has the same number of generic args,
    // with the same bounds.

    /*
    let out_toks = quote! {
        impl #impl_generics Functor<#output_inner> for #ident #input_ty_generics #where_clause {
            type Input = #input_inner;
            type Output = #output_inner;
            type Mapped = #ident #output_ty_generics;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                #fn_body
            }
        }
    };
    println!("{out_toks}");
    out.append_all(out_toks);
*/
}

/// Creates a temporary name for a unnamed field. Used to ensure consistency between declaration
/// and usage
fn make_temporary_ident(field: &Field, index: usize) -> Ident {
    Ident::new(&format!("tmp_{index}"), field.ty.span())
}

/// Makes a line like
///
/// ```rust
/// Ident { x, y, z }
/// ```
///
/// or
///
/// ```rust
/// Ident(tmp_0, tmp_1)
/// ```
///
/// or
///
/// ```rust
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

/// For a given field, decides if it should be fmapped or not, and returns the appropriate
/// expression
fn make_field<'ast>(
    named: bool,
    field: &AField<'ast>,
    inner: &AType<'ast>,
    output_inner: TokenStream2,
) -> TokenStream2 {
    let has_inner = field.types.iter().any(|ty| ty == inner);
    let ident = &field.ident;

    // This logic could be simplified further, but for the sake of clarity, I'd rather keep it in
    // separate cases like this
    if named {
        if has_inner {
            quote! { #ident: Functor::<#output_inner>::fmap(#ident, f) }
        } else {
            quote! { #ident }
        }
    } else {
        if has_inner {
            quote! { Functor::<#output_inner>::fmap(#ident, f) }
        } else {
            quote! { #ident }
        }
    }
}

/// Makes a list of transformed fields, with the appropriate delimiter type based on whether the
/// fields are named or unnamed
fn make_variant_constructor<'ast>(
    container: TokenStream2,
    variant: &AVariant<'ast>,
    inner: &AType<'ast>,
    output_inner: TokenStream2,
) -> TokenStream2 {
    if variant.unit {
        return container;
    }

    let fields = variant
        .fields
        .iter()
        .map(|field| make_field(variant.named, field, inner, output_inner.clone()));
    if variant.named {
        quote! { #container { #(#fields),* } }
    } else {
        quote! { #container ( #(#fields),* ) }
    }
}

/// Writes the body of the functor implementation
fn make_fn_body<'ast>(
    lattice: &ANodes<'ast>,
    container: &ANode<'ast>,
    inner: &AType<'ast>,
    output_inner: TokenStream2,
) -> TokenStream2 {
    let mut out = TokenStream2::new();

    /*
    match container {
        Node::Struct(s) => {
            let destructure = make_destructure_fields(quote! { Self }, &s.fields);
            out.append_all(quote! { let #destructure = self; });

            let inner = inner.ident();
            let constructor = make_constructor_fields(
                lattice,
                quote! { Self::Mapped },
                &s.fields,
                inner,
                output_inner,
            );
            out.append_all(constructor)
        }
        Node::Enum(e) => {
            let variants = e.variants.iter().map(|variant| {
                let ident = &variant.ident;
                let destructure = make_destructure_fields(quote! { Self::#ident }, &variant.fields);

                let inner = inner.ident();
                let constructor = make_constructor_fields(
                    lattice,
                    quote! { Self::Mapped::#ident },
                    &variant.fields,
                    inner,
                    output_inner.clone(),
                );

                quote! {
                    #destructure => #constructor
                }
            });

            out.append_all(quote! {
                match self {
                    #(#variants),*
                }
            })
        }
    };
*/

    out
}

/// Emits all the code we need to generate into a TokenStream representing the interior of the
/// module.
pub fn emit<'ast>(out: &mut TokenStream2, lattice: &ANodes<'ast>) {
    // Emit all base cases
    for node in lattice.0.values() {
        emit_base_case(out, node);
    }

    // Emit all inductive cases
    /*
    for (node_name, children) in lattice.0.iter() {
        let container = lattice.0.get(node_name).unwrap();
        for inner in children.iter().map(|c| nodes.0.get(c).unwrap()) {
            if container.ident() != inner.ident() {
                emit_inductive_case(out, lattice, container, inner);
            }
        }
    }
*/
}
