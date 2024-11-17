use std::collections::HashMap;
use std::collections::HashSet;
use std::error::Error;
use std::fmt::Display;
use std::str::FromStr;

use proc_macro::TokenStream;
use proc_macro::TokenTree;

use syn::visit;
use syn::visit::Visit;
use syn::Fields;
use syn::GenericArgument;
use syn::Generics;
use syn::Ident;
use syn::ItemEnum;
use syn::ItemMod;
use syn::ItemStruct;
use syn::PathArguments;
use syn::Type;

#[cfg(feature = "functor")]
mod functor;
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

/// All the typeclasses we support
#[derive(PartialEq, Eq, Hash)]
enum Typeclass {
    #[cfg(feature = "functor")]
    Functor,
}

#[derive(Debug)]
enum TypeclassParseError {
    BadName(String),
}

impl Display for TypeclassParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeclassParseError::BadName(name) => write!(f, "unexpected typeclass: {name}"),
        }
    }
}

impl Error for TypeclassParseError {}

impl FromStr for Typeclass {
    type Err = TypeclassParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            #[cfg(feature = "functor")]
            "Functor" => Ok(Typeclass::Functor),
            other => Err(TypeclassParseError::BadName(other.into())),
        }
    }
}

/// A parsed set of typeclasses we should generate. If no typeclasses are specified, we should
/// enable all the ones we have features for.
struct EnabledTypeclasses(Option<HashSet<Typeclass>>);

/// Turns a ParseResult<T> into a T, breaking from the loop if None, and panicking if Some(Err)
macro_rules! break_none {
    ($e:expr) => {
        match $e {
            None => break,
            Some(Err(e)) => panic!("{e}"),
            Some(Ok(t)) => t,
        }
    };
}

/// Expects attrs to be formatted as a comma-separated list of idents. Calls `f` on each ident in
/// the list.
fn for_each_ident(attrs: TokenStream, f: &mut impl FnMut(proc_macro::Ident)) {
    let iter = &mut attrs.into_iter();
    loop {
        let ident = break_none!(syntax::ident(&mut TokenStream::new(), iter));

        f(ident);

        break_none!(syntax::punct(
            &mut TokenStream::new(),
            iter,
            &[proc_macro::Punct::new(',', proc_macro::Spacing::Alone)]
        ));
    }
}

impl EnabledTypeclasses {
    fn parse_attrs(attrs: TokenStream) -> Self {
        let mut hs = HashSet::new();

        for_each_ident(attrs, &mut |ident| {
            let typeclass = Typeclass::from_str(&ident.to_string()).expect("parsing typeclass");
            hs.insert(typeclass);
        });

        if hs.len() > 0 {
            Self(Some(hs))
        } else {
            Self(None)
        }
    }

    #[cfg(feature = "functor")]
    fn functor(&self) -> bool {
        match &self.0 {
            None => true,
            Some(hs) => hs.contains(&Typeclass::Functor),
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
pub fn ast(attrs: TokenStream, item: TokenStream) -> TokenStream {
    let enabled = EnabledTypeclasses::parse_attrs(attrs);

    let r#mod: ItemMod = syn::parse(item.clone()).expect("must be applied to module");

    let mut nodes = Nodes::default();
    nodes.visit_item_mod(&r#mod);

    let graph = make_graph(&nodes);
    let lattice = make_lattice(graph);

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

    // The reason we're doing this is to be able to output directly into the inner part of the
    // module, without having to round-trip through syn first.
    let group = {
        let mut out = group.stream().into();

        #[cfg(feature = "functor")]
        if enabled.functor() {
            functor::emit(&mut out, &nodes, &lattice);
        }

        proc_macro::Group::new(proc_macro::Delimiter::Brace, out.into())
    };
    out.extend([proc_macro::TokenTree::from(group)]);

    // There shouldn't be anything left in the iterator
    if let Some(remaining) = iter.next() {
        panic!("unexpected continuation of stream after module: {remaining}");
    }

    out
}

/// Helper macro for manually writing Functor instances, avoiding coherence conflicts by manually
/// specifying what types it will apply to. All this macro does is replace all instances of the
/// string `_T_` in idents with all of the idents it's passed as arguments.
#[proc_macro_attribute]
pub fn specialize(attrs: TokenStream, item: TokenStream) -> TokenStream {
    let mut out = TokenStream::new();

    const MAGIC: &str = "_T_";
    for_each_ident(attrs, &mut |ident| {
        out.extend(item.clone().into_iter().map(|token| match token {
            TokenTree::Ident(i) if i.to_string().contains(MAGIC) => {
                TokenTree::Ident(proc_macro::Ident::new(
                    &i.to_string().replace("_T_", &ident.to_string()),
                    ident.span(),
                ))
            }
            otherwise => otherwise,
        }));
    });

    out
}
