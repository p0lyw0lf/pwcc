use std::collections::HashMap;
use std::collections::HashSet;

use proc_macro::TokenStream;

use syn::visit;
use syn::visit::Visit;
use syn::Fields;
use syn::GenericArgument;
use syn::Ident;
use syn::ItemEnum;
use syn::ItemMod;
use syn::ItemStruct;
use syn::PathArguments;
use syn::Type;

enum Node<'ast> {
    Struct(&'ast ItemStruct),
    Enum(&'ast ItemEnum),
}

/// Collects all the structs/enums for further processing
#[derive(Default)]
struct GetAllNodes<'ast>(HashMap<String, Node<'ast>>);

impl<'ast> Visit<'ast> for GetAllNodes<'ast> {
    fn visit_item_struct(&mut self, node: &'ast ItemStruct) {
        self.0.insert(node.ident.to_string(), Node::Struct(node));
        visit::visit_item_struct(self, node);
    }
    fn visit_item_enum(&mut self, node: &'ast ItemEnum) {
        self.0.insert(node.ident.to_string(), Node::Enum(node));
        visit::visit_item_enum(self, node);
    }
}

/// Creates a graph, where an edge A -> B represents "type A has type B as a child"
fn make_graph<'ast>(nodes: &GetAllNodes<'ast>) -> HashMap<String, Vec<String>> {
    let mut out = HashMap::new();

    for (ident, node) in nodes.0.iter() {
        let mut children = Vec::<String>::new();
        let mut extend_from_fields = |fields: &Fields| match fields {
            Fields::Named(fields) => {
                for field in fields.named.iter() {
                    children.extend(
                        type_to_idents(&field.ty)
                            .into_iter()
                            .map(|ident| ident.to_string())
                            .filter(|ident| nodes.0.contains_key(ident)),
                    );
                }
            }
            Fields::Unnamed(fields) => {
                for field in fields.unnamed.iter() {
                    children.extend(
                        type_to_idents(&field.ty)
                            .into_iter()
                            .map(|ident| ident.to_string())
                            .filter(|ident| nodes.0.contains_key(ident)),
                    );
                }
            }
            Fields::Unit => {}
        };
        match node {
            Node::Struct(s) => extend_from_fields(&s.fields),
            Node::Enum(e) => {
                for case in e.variants.iter() {
                    extend_from_fields(&case.fields);
                }
            }
        };
        out.insert(ident.clone(), children);
    }

    out
}

fn type_to_idents<'ast>(ty: &'ast Type) -> Vec<Ident> {
    match ty {
        Type::Paren(ty) => type_to_idents(&ty.elem),
        Type::Path(ty) => {
            // Only try to map over things in the same module, which never have more than one
            // segment in their path
            if ty.path.segments.len() == 1 {
                let segment = ty.path.segments.iter().next().unwrap();
                let mut out = Vec::from([segment.ident.clone()]);
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in args.args.iter() {
                        if let GenericArgument::Type(ty) = arg {
                            out.extend(type_to_idents(&ty));
                        }
                    }
                }
                out
            } else {
                Vec::new()
            }
        }
        _ => unimplemented!(),
    }
}

/// Repeatedly propagate upwards, making so that if there exists a path from
/// A -...-> B in the original graph, the output lattice will have a direct edge A -> B
/// We do this by repeatedly collapsing all A -> B -> C edges into an A -> C edge.
fn make_lattice<'ast>(graph: HashMap<String, Vec<String>>) -> HashMap<String, HashSet<String>> {
    let mut lattice = HashMap::<String, HashSet<String>>::new();

    // initialize the lattice from the graph
    for (ident, children) in graph.into_iter() {
        lattice.insert(ident, children.into_iter().collect());
    }

    let mut changed = true;
    while changed {
        let mut next = HashMap::with_capacity(lattice.len());
        changed = false;
        for (ident, children) in lattice.iter() {
            // Expand reachable set by 1 step
            let next_children = children
                .iter()
                .filter_map(|i| lattice.get(i))
                .fold(children.clone(), |acc, more| {
                    acc.union(more).map(Clone::clone).collect()
                });
            if children.len() != next_children.len() {
                changed = true;
            }
            next.insert(ident.clone(), next_children);
        }
        lattice = next;
    }

    lattice
}

#[proc_macro_attribute]
pub fn ast(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let r#mod: ItemMod = syn::parse(item.clone()).unwrap();

    let mut nodes = GetAllNodes::default();
    nodes.visit_item_mod(&r#mod);

    let graph = make_graph(&nodes);
    let lattice = make_lattice(graph);

    println!("{lattice:?}");

    item
}
