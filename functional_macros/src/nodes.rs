use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::ops::Deref;

use syn::spanned::Spanned;
use syn::visit;
use syn::visit::Visit;
use syn::Field;
use syn::Fields;
use syn::GenericArgument;
use syn::GenericParam;
use syn::Generics;
use syn::Ident;
use syn::ItemEnum;
use syn::ItemStruct;
use syn::PathArguments;

enum BaseNode<'ast> {
    Struct(&'ast ItemStruct),
    Enum(&'ast ItemEnum),
}

pub mod coherence;
pub mod lattice;
pub mod scc;

/// Maps ident string to original definition
#[derive(Default)]
struct BaseNodes<'ast>(BTreeMap<&'ast Ident, BaseNode<'ast>>);

impl<'ast> Deref for BaseNodes<'ast> {
    type Target = BTreeMap<&'ast Ident, BaseNode<'ast>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'ast> Visit<'ast> for BaseNodes<'ast> {
    fn visit_item_struct(&mut self, node: &'ast ItemStruct) {
        self.0.insert(&node.ident, BaseNode::Struct(node));
        visit::visit_item_struct(self, node);
    }
    fn visit_item_enum(&mut self, node: &'ast ItemEnum) {
        self.0.insert(&node.ident, BaseNode::Enum(node));
        visit::visit_item_enum(self, node);
    }
}

/// All of the identifiers used inside a generic context. E.g., if we have a
/// struct definition like:
///
/// ```rust,ignore
/// enum Result<T, E> {
///     Ok(T),
///     Err(E),
/// }
/// ```
///
/// Then the GenericContext inside the `Result` body is `<T, E>`, and the GenericContext used by
/// the `Ok` variant is `T`.
#[derive(Debug, Default, PartialEq)]
pub struct GenericContext<'ast> {
    lifetimes: HashSet<&'ast Ident>,
    types: HashSet<&'ast Ident>,
    consts: HashSet<&'ast Ident>,
}

impl<'ast> GenericContext<'ast> {
    pub fn has_lifetime(&self, i: &'ast Ident) -> bool {
        self.lifetimes.contains(i)
    }
    pub fn has_type(&self, i: &'ast Ident) -> bool {
        self.types.contains(i)
    }
    pub fn has_const(&self, i: &'ast Ident) -> bool {
        self.consts.contains(i)
    }
    pub fn intersects(&self, other: &GenericContext<'ast>) -> bool {
        !self.types.is_disjoint(&other.types)
            || !self.lifetimes.is_disjoint(&other.lifetimes)
            || !self.consts.is_disjoint(&other.consts)
    }
}

/// Collects all the idents found in a Generics node, which is found at a
/// top-level definition site. In the previous example with `Result`, this would
/// be the `Result<T, E>` part.
pub fn generics_collect_context<'ast>(generics: &'ast Generics) -> GenericContext<'ast> {
    let mut out = GenericContext::default();

    for param in generics.params.iter() {
        match param {
            GenericParam::Lifetime(lp) => out.lifetimes.insert(&lp.lifetime.ident),
            GenericParam::Type(tp) => out.types.insert(&tp.ident),
            GenericParam::Const(cp) => out.consts.insert(&cp.ident),
        };
    }

    out
}

/// Collects all the idents found in an instantiation, given a parent `ctx`.
/// In the previous example with `Result`, this would be the `Ok(T)` part.
pub fn instantiation_collect_context<'ast>(
    ctx: &GenericContext<'ast>,
    args: impl Iterator<Item = &'ast GenericArgument>,
) -> GenericContext<'ast> {
    struct Collect<'ast, 'outer> {
        parent_ctx: &'outer GenericContext<'ast>,
        ctx: GenericContext<'ast>,
    }

    impl<'ast, 'outer> Visit<'ast> for Collect<'ast, 'outer> {
        fn visit_lifetime(&mut self, l: &'ast syn::Lifetime) {
            if self.parent_ctx.has_lifetime(&l.ident) {
                self.ctx.lifetimes.insert(&l.ident);
            }
            visit::visit_lifetime(self, l);
        }

        fn visit_type_path(&mut self, t: &'ast syn::TypePath) {
            if let Some(first) = t.path.segments.first() {
                if t.qself.is_none() && self.parent_ctx.has_type(&first.ident) {
                    self.ctx.types.insert(&first.ident);
                }
            }
            visit::visit_type_path(self, t);
        }

        fn visit_ident(&mut self, i: &'ast syn::Ident) {
            if self.parent_ctx.has_const(&i) {
                self.ctx.consts.insert(&i);
            }
            visit::visit_ident(self, i);
        }
    }

    let mut c = Collect {
        parent_ctx: ctx,
        ctx: GenericContext::default(),
    };
    for arg in args {
        c.visit_generic_argument(arg);
    }
    c.ctx
}

/// An "annotated field" in a struct/enum annotated with the requisite data to do introspection on.
#[derive(Debug)]
pub struct AField<'ast> {
    /// Corresponds to the declared ident if in a named struct/enum variant, otherwise corresponds to an anonymous ident if in an unnamed struct/enum variant.
    pub ident: Cow<'ast, Ident>,
    /// All the types that appear in this field. We need this to be a collection instead of just one because it may track _all_ types that are at or below the one defined for the field.
    pub types: HashSet<AType<'ast>>,
}

/// An "annotated type" of a field that corresponds to one of our lattice types.
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct AType<'ast> {
    /// An index into the outer Nodes structure
    pub ident: &'ast Ident,
    /// The instantiation of generic arguments for this type. May correspond to the instantion
    /// defined in the field, or may be created as a result of propagating child relationships.
    pub instantiation: Vec<Cow<'ast, GenericArgument>>,
}

fn convert_field<'ast>(nodes: &BaseNodes<'ast>, field: &'ast Field, index: usize) -> AField<'ast> {
    struct TypeVisitor<'ast, 'a> {
        /// The types we're looking for
        nodes: &'a BaseNodes<'ast>,
        /// All the collected types we've seen so far
        types: HashSet<AType<'ast>>,
    }
    impl<'ast, 'a> Visit<'ast> for TypeVisitor<'ast, 'a> {
        fn visit_type_path(&mut self, ty: &'ast syn::TypePath) {
            // If it's just a single name in the path, it's highly likely it's part of the AST
            // we're building. Check for this.
            if ty.qself.is_none() && ty.path.segments.len() == 1 {
                let segment = ty.path.segments.first().unwrap();
                let ident = &segment.ident;
                if self.nodes.contains_key(ident) {
                    self.types.insert(AType {
                        ident,
                        instantiation: if let PathArguments::AngleBracketed(args) =
                            &segment.arguments
                        {
                            args.args.iter().map(Cow::Borrowed).collect()
                        } else {
                            Vec::default()
                        },
                    });
                }
            }

            visit::visit_type_path(self, ty);
        }
    }

    let mut visitor = TypeVisitor {
        nodes,
        types: HashSet::new(),
    };
    visitor.visit_field(field);

    let ident = field.ident.as_ref().map_or_else(
        || Cow::Owned(Ident::new(&format!("tmp_{index}"), field.ty.span())),
        Cow::Borrowed,
    );

    AField {
        ident,
        types: visitor.types,
    }
}

/// An "annotated variant", representing either a standalone struct or an enum variant
#[derive(Debug)]
pub struct AVariant<'ast> {
    /// The original Ident token when the struct was defined
    pub ident: &'ast Ident,
    /// Whether the fields are named or unnamed
    pub named: bool,
    /// Whether this is a unit struct or not
    pub unit: bool,
    /// All the fields in the struct
    pub fields: Vec<AField<'ast>>,
}

fn convert_variant<'nodes, 'ast>(
    nodes: &'nodes BaseNodes<'ast>,
    ident: &'ast Ident,
    fields: &'ast Fields,
) -> AVariant<'ast> {
    let named = matches!(fields, syn::Fields::Named(_));
    let unit = matches!(fields, syn::Fields::Unit);
    let fields = fields
        .iter()
        .enumerate()
        .map(|(index, field)| convert_field(nodes, field, index))
        .collect();

    AVariant {
        ident,
        named,
        unit,
        fields,
    }
}

/// An "annotated struct", which we model as consisting of a single variant
#[derive(Debug)]
pub struct AStruct<'ast> {
    pub data: AVariant<'ast>,
    /// The generic context defined by this struct
    pub ctx: GenericContext<'ast>,
    /// The exact order of generic arguments
    pub generics: &'ast Generics,
}

/// An "annotated enum", representing a standalone enum
#[derive(Debug)]
pub struct AEnum<'ast> {
    /// The original Ident token when the enum was defined
    pub ident: &'ast Ident,
    /// All of the variants in the enum
    pub variants: Vec<AVariant<'ast>>,
    /// The generic context defined by this enum
    pub ctx: GenericContext<'ast>,
    /// The exact order of generic arguments
    pub generics: &'ast Generics,
}

#[derive(Debug)]
pub enum ANode<'ast> {
    Struct(AStruct<'ast>),
    Enum(AEnum<'ast>),
}

impl<'ast> ANode<'ast> {
    pub fn ident(&self) -> &'ast Ident {
        match self {
            ANode::Struct(s) => s.data.ident,
            ANode::Enum(s) => s.ident,
        }
    }

    pub fn generics(&self) -> &'ast Generics {
        match self {
            ANode::Struct(s) => s.generics,
            ANode::Enum(s) => s.generics,
        }
    }

    pub fn ctx(&self) -> &GenericContext {
        match self {
            ANode::Struct(s) => &s.ctx,
            ANode::Enum(s) => &s.ctx,
        }
    }

    pub fn fields(&self) -> Box<dyn Iterator<Item = &'_ AField<'ast>> + '_> {
        match self {
            ANode::Struct(s) => Box::new(s.data.fields.iter()),
            ANode::Enum(s) => Box::new(
                s.variants
                    .iter()
                    .map(|variant| variant.fields.iter())
                    .flatten(),
            ),
        }
    }

    pub fn tys(&self) -> impl Iterator<Item = &'_ AType<'ast>> + '_ {
        self.fields().map(|field| field.types.iter()).flatten()
    }

    pub fn fields_mut(&mut self) -> Box<dyn Iterator<Item = &'_ mut AField<'ast>> + '_> {
        match self {
            ANode::Struct(s) => Box::new(s.data.fields.iter_mut()),
            ANode::Enum(s) => Box::new(
                s.variants
                    .iter_mut()
                    .map(|variant| variant.fields.iter_mut())
                    .flatten(),
            ),
        }
    }
}

fn convert_struct<'nodes, 'ast>(
    nodes: &'nodes BaseNodes<'ast>,
    item_struct: &'ast ItemStruct,
) -> AStruct<'ast> {
    let data = convert_variant(nodes, &item_struct.ident, &item_struct.fields);
    let ctx = generics_collect_context(&item_struct.generics);

    AStruct {
        data,
        ctx,
        generics: &item_struct.generics,
    }
}

fn convert_enum<'ast>(nodes: &BaseNodes<'ast>, item_enum: &'ast ItemEnum) -> AEnum<'ast> {
    let ident = &item_enum.ident;
    let variants = item_enum
        .variants
        .iter()
        .map(|variant| convert_variant(nodes, &variant.ident, &variant.fields))
        .collect();
    let ctx = generics_collect_context(&item_enum.generics);

    AEnum {
        ident,
        variants,
        ctx,
        generics: &item_enum.generics,
    }
}

/// INVARIANT: \forall i. nodes[i].ident() == i
#[derive(Default, Debug)]
pub struct ANodes<'ast>(pub BTreeMap<&'ast Ident, ANode<'ast>>);

impl<'ast> Deref for ANodes<'ast> {
    type Target = BTreeMap<&'ast Ident, ANode<'ast>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn make_nodes<'ast>(m: &'ast syn::ItemMod) -> ANodes<'ast> {
    // Pass 1: Read in most basic data by visiting all structs/enums
    let mut base = BaseNodes::default();
    base.visit_item_mod(m);

    // Pass 2: Annotate nodes with generic context data for their direct children.
    let nodes = ANodes(
        base.iter()
            .map(|(key, node)| {
                (
                    *key,
                    match node {
                        BaseNode::Struct(s) => ANode::Struct(convert_struct(&base, s)),
                        BaseNode::Enum(e) => ANode::Enum(convert_enum(&base, e)),
                    },
                )
            })
            .collect(),
    );

    // For more complicated passes, see the `lattice` and `scc` modules.
    nodes
}

#[cfg(test)]
mod test {
    //! Just some test utilities for other modules to use

    use std::borrow::Borrow;
    use std::borrow::Cow;

    use proc_macro2::Span;
    use syn::Generics;

    use crate::nodes::lattice::make_lattice;
    use crate::nodes::AField;
    use crate::nodes::ANodes;
    use crate::nodes::AStruct;
    use crate::nodes::AType;
    use crate::nodes::AVariant;

    use super::*;

    struct NodeBuilder {
        /// Because so much relies on &'ast Ident, we need to make an arena for those
        label_arena: Vec<Ident>,
        empty_generics: Generics,
    }

    impl NodeBuilder {
        /// Initializes the NodeBuilder arena with the given labels. These labels will be referred
        /// to be index in the `add_node` function.
        fn with_labels(labels: &[impl Borrow<str>]) -> Self {
            Self {
                label_arena: labels
                    .iter()
                    .map(|label| Ident::new(label.borrow(), Span::call_site()))
                    .collect(),
                empty_generics: Default::default(),
            }
        }

        fn add_node<'ast>(&'ast self, nodes: &mut ANodes<'ast>, label: usize, edges: &[usize]) {
            let ident = &self.label_arena[label];
            let node = ANode::Struct(AStruct {
                data: AVariant {
                    ident,
                    named: true,
                    unit: false,
                    fields: edges
                        .iter()
                        .map(|edge| {
                            let ident = &self.label_arena[*edge];
                            AField {
                                ident: Cow::Borrowed(ident),
                                types: [AType {
                                    ident,
                                    instantiation: Default::default(),
                                }]
                                .into_iter()
                                .collect(),
                            }
                        })
                        .collect(),
                },
                ctx: Default::default(),
                generics: &self.empty_generics,
            });

            nodes.0.insert(ident, node);
        }
    }

    /// edges[i] contains a list of all other indicies of vertices for outgoing edges
    pub fn run_test(edges: &[&[usize]], test_fn: impl FnOnce(ANodes<'_>, Vec<Ident>)) {
        let mut nodes = ANodes::default();
        let builder = NodeBuilder::with_labels(
            (0..edges.len())
                .map(|i| format!("Node{i}"))
                .collect::<Vec<_>>()
                .as_slice(),
        );

        for (i, edges) in edges.iter().enumerate() {
            builder.add_node(&mut nodes, i, edges);
        }

        test_fn(nodes, builder.label_arena.clone());
    }
}
