use std::collections::HashSet;
use std::ops::Deref;

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::ToTokens;
use quote::TokenStreamExt;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::visit_mut;
use syn::visit_mut::VisitMut;
use syn::GenericArgument;
use syn::GenericParam;
use syn::Generics;
use syn::Ident;
use syn::Token;
use syn::WhereClause;

use crate::nodes::instantiation_collect_context;
use crate::nodes::AField;
use crate::nodes::ANode;
use crate::nodes::ANodes;
use crate::nodes::AType;
use crate::nodes::AVariant;
use crate::nodes::GenericContext;

/// Visitor that will append a suffix to all generic parameters in a set of idents
struct AddSuffix<'ast, 'suffix, 'hashset> {
    // Should be CamelCase
    suffix: &'suffix str,
    ctx: &'hashset GenericContext<'ast>,
}

impl<'ast, 'suffix, 'hashset> VisitMut for AddSuffix<'ast, 'suffix, 'hashset> {
    fn visit_lifetime_mut(&mut self, l: &mut syn::Lifetime) {
        if self.ctx.has_lifetime(&l.ident) {
            l.ident = Ident::new(
                &format!("{}_{}", l.ident, self.suffix.to_lowercase()),
                l.ident.span(),
            );
        }
        visit_mut::visit_lifetime_mut(self, l);
    }

    fn visit_type_param_mut(&mut self, t: &mut syn::TypeParam) {
        if self.ctx.has_type(&t.ident) {
            t.ident = Ident::new(&format!("{}{}", t.ident, self.suffix), t.ident.span());
        }
    }

    fn visit_type_path_mut(&mut self, t: &mut syn::TypePath) {
        if let Some(first) = t.path.segments.first_mut() {
            if t.qself.is_none() && self.ctx.has_type(&first.ident) {
                first.ident = Ident::new(
                    &format!("{}{}", first.ident, self.suffix),
                    first.ident.span(),
                );
            }
        }
        visit_mut::visit_type_path_mut(self, t);
    }

    fn visit_ident_mut(&mut self, i: &mut syn::Ident) {
        if self.ctx.has_const(&i) {
            *i = Ident::new(&format!("{}_{}", i, self.suffix.to_uppercase()), i.span());
        }
        visit_mut::visit_ident_mut(self, i);
    }
}

/// Adds a string to the idents of all of the given generic arguments present in `ctx`, returning the resulting generics.
/// If `remove` is true, removes all parameters that don't correspond to anything in `ctx`.
fn generics_add_suffix<'ast>(
    generics: &Generics,
    suffix: &str,
    ctx: &GenericContext<'ast>,
    remove: bool,
) -> Generics {
    let mut generics = generics.clone();
    let mut v = AddSuffix { suffix, ctx };
    generics.params = generics
        .params
        .into_iter()
        .filter_map(|param| match param {
            GenericParam::Lifetime(l) if remove && !ctx.has_lifetime(&l.lifetime.ident) => None,
            GenericParam::Type(t) if remove && !ctx.has_type(&t.ident) => None,
            GenericParam::Const(c) if remove && !ctx.has_const(&c.ident) => None,
            mut param => {
                v.visit_generic_param_mut(&mut param);
                Some(param)
            }
        })
        .collect();
    generics
}

/// Adds a string to all identifiers found in `ctx`
fn instantiation_add_suffix<'ast>(
    instantiation: impl Iterator<Item = &'ast GenericArgument> + 'ast,
    suffix: &'ast str,
    ctx: &'ast GenericContext<'ast>,
) -> impl Iterator<Item = GenericArgument> + 'ast {
    instantiation.map(|arg| {
        let mut arg = arg.clone();
        AddSuffix { suffix, ctx }.visit_generic_argument_mut(&mut arg);
        arg
    })
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
    let ident = node.ident();

    let generics = node.generics();
    let ctx = node.ctx();

    // If this is a recursive type, we should _not_ generate a base case, and instead let the
    // inductive case take care of that.
    if node.tys().any(|ty| ty.ident == ident) {
        return;
    }

    let input_generics = generics_add_suffix(generics, "Input", &ctx, false);
    let output_generics = generics_add_suffix(generics, "Output", &ctx, false);

    let all_generics = generics_merge(&input_generics, &output_generics);

    let (impl_generics, _, where_clause) = all_generics.split_for_impl();
    let (_, input_ty_generics, _) = input_generics.split_for_impl();
    let (_, output_ty_generics, _) = output_generics.split_for_impl();

    let out_toks = quote! {
        impl #impl_generics Functor<#ident #output_ty_generics> for #ident #input_ty_generics #where_clause {
            type Input = #ident #input_ty_generics;
            type Output = #ident #output_ty_generics;
            type Mapped = #ident #output_ty_generics;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                f(self)
            }
        }
    };

    println!("{out_toks}");
    out.append_all(out_toks);
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
    let inner_node = lattice.get(inner.ident).expect("missing node");
    let inner_ident = inner_node.ident();

    let container_ctx = container.ctx();
    let inner_ctx =
        instantiation_collect_context(container_ctx, inner.instantiation.iter().map(Deref::deref));

    let container_generics = container.generics();

    let container_input_generics_partial =
        generics_add_suffix(container_generics, "Input", &inner_ctx, true);
    let container_input_generics_full =
        generics_add_suffix(container_generics, "Input", &inner_ctx, false);
    let container_output_generics =
        generics_add_suffix(container_generics, "Output", &inner_ctx, false);

    let all_generics = generics_merge(
        &container_input_generics_partial,
        &container_output_generics,
    );

    let (impl_generics, _, where_clause) = all_generics.split_for_impl();
    let (_, input_ty_generics, _) = container_input_generics_full.split_for_impl();
    let (_, output_ty_generics, _) = container_output_generics.split_for_impl();

    let inner_instantiation = inner.instantiation.iter().map(Deref::deref);
    let input_inner_instantiation =
        instantiation_add_suffix(inner_instantiation.clone(), "Input", &inner_ctx)
            .collect::<Punctuated<_, Token![,]>>();
    let input_inner = quote! { #inner_ident < #input_inner_instantiation > };

    let output_inner_instantiation =
        instantiation_add_suffix(inner_instantiation, "Output", &inner_ctx)
            .collect::<Punctuated<_, Token![,]>>();
    let output_inner = quote! { #inner_ident < #output_inner_instantiation > };

    let fn_body = make_fn_body(container, inner, output_inner.clone());

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
    container: &ANode<'ast>,
    inner: &AType<'ast>,
    output_inner: TokenStream2,
) -> TokenStream2 {
    let mut out = TokenStream2::new();

    match container {
        ANode::Struct(s) => {
            let destructor = make_variant_destructor(quote! { Self }, &s.data);
            let constructor = make_variant_constructor(
                if s.data.named {
                    quote! { Self::Mapped }
                } else {
                    // TODO: I am not sure why, but for some reason, unnamed structs don't like
                    // doing Self::Mapped(), and prefer their original name instead. Exactly why is
                    // beyond me, but this seems to work, so whatever.
                    s.data.ident.to_token_stream()
                },
                &s.data,
                inner,
                output_inner.clone(),
            );
            out.append_all(quote! {
                let #destructor = self;
                #constructor
            });
        }
        ANode::Enum(e) => {
            let variants = e.variants.iter().map(|variant| {
                let ident = &variant.ident;
                let destructor = make_variant_destructor(quote! { Self::#ident }, variant);

                let constructor = make_variant_constructor(
                    quote! { Self::Mapped::#ident },
                    variant,
                    inner,
                    output_inner.clone(),
                );

                quote! {
                    #destructor => #constructor
                }
            });

            out.append_all(quote! {
                match self {
                    #(#variants),*
                }
            })
        }
    };

    if container.ident() == inner.ident {
        quote! {
            let out = #out;
            f(out)
        }
    } else {
        out
    }
}

/// Emits all the code we need to generate into a TokenStream representing the interior of the
/// module.
pub fn emit<'ast>(out: &mut TokenStream2, lattice: &ANodes<'ast>) {
    // Emit all base cases
    for node in lattice.values() {
        emit_base_case(out, node);
    }

    // Emit all inductive cases
    for container in lattice.values() {
        let types = container
            .fields()
            .map(|field| field.types.iter())
            .flatten()
            .collect::<HashSet<_>>();
        for inner in types.into_iter() {
            emit_inductive_case(out, lattice, container, inner)
        }
    }
}
