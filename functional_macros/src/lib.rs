use std::collections::HashMap;
use std::collections::HashSet;

use proc_macro::TokenStream;

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
use syn::Type;
use syn::TypeParam;

mod syntax;

enum Node<'ast> {
    Struct(&'ast ItemStruct),
    Enum(&'ast ItemEnum),
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

/// Appends a string to the idents of all of the given generic arguments, returning the resulting generics
fn generics_add_suffix(generics: &Generics, suffix: &str) -> Generics {
    let mut generics = generics.clone();
    struct Append<'a>(&'a str);
    impl<'a> Append<'a> {
        fn append_to(&self, i: &mut Ident) {
            *i = Ident::new(&{ i.to_string() + self.0 }, i.span())
        }
    }
    impl<'a> VisitMut for Append<'a> {
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

    Append(suffix).visit_generics_mut(&mut generics);
    generics
}

/// Emits the impl Functor<T> for T implementation for the given type, applying generic arguments
/// as applicable
fn emit_base_case<'ast>(out: &mut TokenStream, ident: &Ident, generics: &Generics) {
    let input_generics = generics_add_suffix(generics, "Input");
    let output_generics = generics_add_suffix(generics, "Output");
}

/// Emits all the code we need to generate into a TokenStream representing the interior of the
/// module.
fn emit_into_mod<'ast>(out: &mut TokenStream, nodes: Nodes<'ast>, lattice: Lattice) {
    for node in nodes.0.values() {
        match node {
            Node::Struct(s) => emit_base_case(out, &s.ident, &s.generics),
            Node::Enum(e) => emit_base_case(out, &e.ident, &e.generics),
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
        let mut out = group.stream();
        emit_into_mod(&mut out, nodes, lattice);
        proc_macro::Group::new(proc_macro::Delimiter::Brace, out)
    };
    out.extend([proc_macro::TokenTree::from(group)]);

    // There shouldn't be anything left in the iterator
    if let Some(remaining) = iter.next() {
        panic!("unexpected continuation of stream after module: {remaining}");
    }

    out
}
