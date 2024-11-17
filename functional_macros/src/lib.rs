use std::collections::HashMap;
use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

use quote::quote;
use quote::TokenStreamExt;
use syn::visit;
use syn::visit::Visit;
use syn::visit_mut;
use syn::visit_mut::VisitMut;
use syn::ConstParam;
use syn::Fields;
use syn::GenericArgument;
use syn::Generics;
use syn::Ident;
use syn::ItemEnum;
use syn::ItemMod;
use syn::ItemStruct;
use syn::Lifetime;
use syn::PathArguments;
use syn::Token;
use syn::Type;
use syn::TypeParam;
use syn::WhereClause;

mod syntax;

enum Node<'ast> {
    Struct(&'ast ItemStruct),
    Enum(&'ast ItemEnum),
}

impl<'ast> Node<'ast> {
    fn ident(&self) -> &'ast Ident {
        match self {
            Node::Struct(s) => &s.ident,
            Node::Enum(e) => &e.ident,
        }
    }

    fn generics(&self) -> &'ast Generics {
        match self {
            Node::Struct(s) => &s.generics,
            Node::Enum(e) => &e.generics,
        }
    }
}

/// Maps ident string to original definition
#[derive(Default)]
struct Nodes<'ast>(HashMap<String, Node<'ast>>);

impl<'ast> Visit<'ast> for Nodes<'ast> {
    fn visit_item_struct(&mut self, node: &'ast ItemStruct) {
        self.0.insert(node.ident.to_string(), Node::Struct(node));
        visit::visit_item_struct(self, node);
    }
    fn visit_item_enum(&mut self, node: &'ast ItemEnum) {
        self.0.insert(node.ident.to_string(), Node::Enum(node));
        visit::visit_item_enum(self, node);
    }
}

/// Maps ident string -> all idents that appear in valid spots within the struct/enum the original
/// ident represents.
#[derive(Default)]
struct Graph(HashMap<String, Vec<String>>);

/// Given a type of a field, returns all the idents that are possibly valid for being "children"
/// types.
fn type_to_idents<'ast>(ty: &'ast Type) -> Vec<Ident> {
    match ty {
        Type::Paren(ty) => type_to_idents(&ty.elem),
        Type::Path(ty) => {
            let mut out = Vec::default();
            // If it's just a single name in the path, it's highly likely it's something in our
            // module, so add it
            if ty.path.segments.len() == 1 {
                let segment = ty.path.segments.iter().next().unwrap();
                out.push(segment.ident.clone());
            }

            // But also, it could be something like Option<T> or Vec<T>,
            // or even std::option::Option<T>, in which case we should check
            // all generic arguments just to make sure.
            for segment in ty.path.segments.iter() {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in args.args.iter() {
                        if let GenericArgument::Type(ty) = arg {
                            out.extend(type_to_idents(&ty));
                        }
                    }
                }
            }
            out
        }
        _ => Vec::new(),
    }
}

impl Graph {
    fn extend_from_ty<'ast>(&mut self, nodes: &Nodes<'ast>, ident: &String, ty: &Type) {
        let children = self.0.entry(ident.to_string()).or_default();
        children.extend(
            type_to_idents(ty)
                .into_iter()
                .map(|i| i.to_string())
                .filter(|i| nodes.0.contains_key(i)),
        );
    }

    fn extend_from_fields<'ast>(&mut self, nodes: &Nodes<'ast>, ident: &String, fields: &Fields) {
        match fields {
            Fields::Named(fields) => {
                for field in fields.named.iter() {
                    self.extend_from_ty(nodes, ident, &field.ty);
                }
            }
            Fields::Unnamed(fields) => {
                for field in fields.unnamed.iter() {
                    self.extend_from_ty(nodes, ident, &field.ty);
                }
            }
            Fields::Unit => {}
        }
    }
}

/// Creates a graph, where an edge A -> B represents "type A has type B as a child"
fn make_graph(nodes: &Nodes<'_>) -> Graph {
    let mut out = Graph::default();

    for (ident, node) in nodes.0.iter() {
        match node {
            Node::Struct(s) => out.extend_from_fields(nodes, ident, &s.fields),
            Node::Enum(e) => {
                for case in e.variants.iter() {
                    out.extend_from_fields(nodes, ident, &case.fields);
                }
            }
        };
    }

    out
}

#[derive(Default, Debug)]
struct Lattice(HashMap<String, HashSet<String>>);

/// Repeatedly propagate upwards, making so that if there exists a path from
/// A -...-> B in the original graph, the output lattice will have a direct edge A -> B
/// We do this by repeatedly collapsing all A -> B -> C edges into an A -> C edge.
fn make_lattice(graph: Graph) -> Lattice {
    let mut lattice = Lattice::default();

    // initialize the lattice from the graph
    for (ident, children) in graph.0.into_iter() {
        lattice.0.insert(ident, children.into_iter().collect());
    }

    let mut changed = true;
    while changed {
        let mut next = HashMap::with_capacity(lattice.0.len());
        changed = false;
        for (ident, children) in lattice.0.iter() {
            // Expand reachable set by 1 step
            let next_children = children
                .iter()
                .filter_map(|i| lattice.0.get(i))
                .fold(children.clone(), |acc, more| {
                    acc.union(more).map(Clone::clone).collect()
                });
            if children.len() != next_children.len() {
                changed = true;
            }
            next.insert(ident.clone(), next_children);
        }
        lattice = Lattice(next);
    }

    lattice
}

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

fn make_fn_body<'ast>(container: &Node<'ast>, inner: &Node<'ast>) -> TokenStream2 {
    quote! { todo!() }
}

/// Emits all the code we need to generate into a TokenStream representing the interior of the
/// module.
fn emit_into_mod<'ast>(out: &mut TokenStream2, nodes: Nodes<'ast>, lattice: Lattice) {
    // Emit all base cases
    for node in nodes.0.values() {
        emit_base_case(out, node.ident(), node.generics());
    }

    // Emit all inductive cases
    for (node_name, children) in lattice.0.into_iter() {
        let container = nodes.0.get(&node_name).unwrap();
        for inner in children.into_iter().map(|c| nodes.0.get(&c).unwrap()) {
            if container.ident() != inner.ident() {
                emit_inductive_case(out, container, inner);
            }
        }
    }
}

/// Turns a syntax::ParseResult<T> into a T, panicking if it's a None or Some(Err)
macro_rules! always {
    ($e:expr) => {{
        match $e {
            Some(Ok(t)) => t,
            Some(Err(e)) => panic!("{e}"),
            None => panic!("unexpected end of stream"),
        }
    }};
}

/// Turns a syntax::ParseResult<T> into a Result<T, ParseError>, panicking if its None
macro_rules! maybe {
    ($e:expr) => {
        match $e {
            Some(v) => v,
            None => panic!("unexpected end of stream"),
        }
    };
}

#[proc_macro_attribute]
pub fn ast(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let r#mod: ItemMod = syn::parse(item.clone()).expect("must be applied to module");

    let mut nodes = Nodes::default();
    nodes.visit_item_mod(&r#mod);

    let graph = make_graph(&nodes);
    let lattice = make_lattice(graph);

    println!("{lattice:?}");

    let mut out = TokenStream::new();
    let iter = &mut item.into_iter();

    // All parse errors should have been handled earlier by syn, just do these assertions for extra safety
    loop {
        let res = maybe!(syntax::outer_attribute(&mut out, iter));
        if res.is_err() {
            break;
        }
    }
    let _ = maybe!(syntax::visibility(&mut out, iter));
    always!(syntax::keyword(&mut out, iter, "mod"));
    let _ = always!(syntax::ident(&mut out, iter));
    let group = always!(syntax::group(iter, proc_macro::Delimiter::Brace));

    let group = {
        let mut out = group.stream().into();
        emit_into_mod(&mut out, nodes, lattice);
        proc_macro::Group::new(proc_macro::Delimiter::Brace, out.into())
    };
    out.extend([proc_macro::TokenTree::from(group)]);

    // There shouldn't be anything left in the iterator
    if let Some(remaining) = iter.next() {
        panic!("unexpected continuation of stream after module: {remaining}");
    }

    out
}
