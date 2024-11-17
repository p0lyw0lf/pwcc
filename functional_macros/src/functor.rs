use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use quote::TokenStreamExt;
use syn::visit_mut;
use syn::visit_mut::VisitMut;
use syn::ConstParam;
use syn::Generics;
use syn::Ident;
use syn::Lifetime;
use syn::Token;
use syn::TypeParam;
use syn::WhereClause;

use crate::Nodes;
use crate::Lattice;
use crate::Node;

/// Visitor that will append a suffix to all generic parameters it finds
struct AddSuffix<'a>(&'a str);
impl<'a> AddSuffix<'a> {
    fn append_to(&self, i: &mut Ident) {
        *i = Ident::new(&{ i.to_string() + self.0 }, i.span())
    }
}
impl<'a> VisitMut for AddSuffix<'a> {
    fn visit_lifetime_mut(&mut self, l: &mut Lifetime) {
        self.append_to(&mut l.ident);
        visit_mut::visit_lifetime_mut(self, l);
    }

    fn visit_type_param_mut(&mut self, tp: &mut TypeParam) {
        // If the bound looks something like Type: Trait, we should return something like
        // TypeOutput: Trait
        if tp.colon_token.is_some() {
            self.append_to(&mut tp.ident);
        }
        // TODO: it's more difficult to handle bounds like I: Iterator<Item = Type>. We could
        // have transformed Type earlier in the param, or Type could be something from external
        // context. For now, let's just always assume it's in external context and
        // (confusingly) fail if it's a more complex bound.
        visit_mut::visit_type_param_mut(self, tp);
    }

    fn visit_const_param_mut(&mut self, cp: &mut ConstParam) {
        self.append_to(&mut cp.ident);
        visit_mut::visit_const_param_mut(self, cp);
    }
}

/// AddSuffixs a string to the idents of all of the given generic arguments, returning the resulting generics
fn generics_add_suffix(generics: &Generics, suffix: &str) -> Generics {
    let mut generics = generics.clone();
    AddSuffix(suffix).visit_generics_mut(&mut generics);
    generics
}

/// Merge two Generics into a single one
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
fn emit_base_case(out: &mut TokenStream2, ident: &Ident, generics: &Generics) {
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
}

/// Emit the impl Functor<Inner> for Container implementation for the given inner type and
/// container, applying all generic arguments as applicable.
fn emit_inductive_case<'ast>(out: &mut TokenStream2, container: &Node<'ast>, inner: &Node<'ast>) {
    let ident = container.ident();
    let generics = container.generics();

    let input_generics = generics_add_suffix(generics, "Input");
    let output_generics = generics_add_suffix(generics, "Output");

    let all_generics = generics_merge(&input_generics, &output_generics);

    let (impl_generics, _, where_clause) = all_generics.split_for_impl();
    let (_, input_ty_generics, _) = input_generics.split_for_impl();
    let (_, output_ty_generics, _) = output_generics.split_for_impl();

    let input_inner_generics = generics_add_suffix(inner.generics(), "Input");
    let output_inner_generics = generics_add_suffix(inner.generics(), "Output");

    let fn_body = make_fn_body(container, inner);

    let inner = inner.ident();

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
    out.append_all(quote! {
        impl #impl_generics Functor<#inner #output_inner_generics> for #ident #input_ty_generics #where_clause {
            type Input = #inner #input_inner_generics;
            type Output = #inner #output_inner_generics;
            type Mapped = #ident #output_ty_generics;
            fn fmap(self, f: &mut impl FnMut(Self::Input) -> Self::Output) -> Self::Mapped {
                #fn_body
            }
        }
    });
}

/// Writes the body of the functor implementation
fn make_fn_body<'ast>(container: &Node<'ast>, inner: &Node<'ast>) -> TokenStream2 {
    quote! { todo!() }
}

/// Emits all the code we need to generate into a TokenStream representing the interior of the
/// module.
pub fn emit<'ast>(out: &mut TokenStream2, nodes: &Nodes<'ast>, lattice: &Lattice) {
    // Emit all base cases
    for node in nodes.0.values() {
        emit_base_case(out, node.ident(), node.generics());
    }

    // Emit all inductive cases
    for (node_name, children) in lattice.0.iter() {
        let container = nodes.0.get(node_name).unwrap();
        for inner in children.iter().map(|c| nodes.0.get(c).unwrap()) {
            if container.ident() != inner.ident() {
                emit_inductive_case(out, container, inner);
            }
        }
    }
}
