use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::hash::Hash;
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
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct GenericContext<'ast> {
    lifetimes: HashSet<&'ast Ident>,
    types: HashSet<&'ast Ident>,
    consts: HashSet<&'ast Ident>,
}

impl<'ast> GenericContext<'ast> {
    pub fn has_lifetime(&self, i: &'ast Ident) -> bool {
        self.lifetimes.contains(i)
    }
    pub fn get_lifetime(&self, i: &Ident) -> Option<&'ast Ident> {
        self.lifetimes.get(i).map(|x| *x)
    }
    pub fn has_type(&self, i: &'ast Ident) -> bool {
        self.types.contains(i)
    }
    pub fn get_type(&self, i: &Ident) -> Option<&'ast Ident> {
        self.types.get(i).map(|x| *x)
    }
    pub fn has_const(&self, i: &'ast Ident) -> bool {
        self.consts.contains(i)
    }
    pub fn get_const(&self, i: &Ident) -> Option<&'ast Ident> {
        self.consts.get(i).map(|x| *x)
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
///
/// NOTE: The lifetimes work out because we can copy the &'ast references out of the parent context.
pub fn instantiation_collect_context<'vec, 'ast>(
    ctx: &GenericContext<'ast>,
    instantiation: impl Iterator<Item = &'vec GenericArgument> + 'vec,
) -> GenericContext<'ast> {
    struct Collect<'ast, 'outer> {
        parent_ctx: &'outer GenericContext<'ast>,
        ctx: GenericContext<'ast>,
    }

    impl<'ast, 'vec, 'outer> Visit<'vec> for Collect<'ast, 'outer> {
        fn visit_lifetime(&mut self, l: &'vec syn::Lifetime) {
            if let Some(ident) = self.parent_ctx.get_lifetime(&l.ident) {
                self.ctx.lifetimes.insert(ident);
            }
            visit::visit_lifetime(self, l);
        }

        fn visit_type_path(&mut self, t: &'vec syn::TypePath) {
            if t.qself.is_none() {
                if let Some(first) = t.path.segments.first() {
                    if let Some(ident) = self.parent_ctx.get_type(&first.ident) {
                        self.ctx.types.insert(ident);
                    }
                }
            }
            visit::visit_type_path(self, t);
        }

        fn visit_ident(&mut self, i: &'vec syn::Ident) {
            if let Some(ident) = self.parent_ctx.get_const(i) {
                self.ctx.consts.insert(ident);
            }
            visit::visit_ident(self, i);
        }
    }

    let mut c = Collect {
        parent_ctx: ctx,
        ctx: GenericContext::default(),
    };
    for arg in instantiation {
        c.visit_generic_argument(arg);
    }
    c.ctx
}

/// An "annotated field" in a struct/enum annotated with the requisite data to do introspection on.
#[derive(Debug)]
pub struct AField<'ast> {
    /// Corresponds to the declared ident if in a named struct/enum variant, otherwise corresponds to an anonymous ident if in an unnamed struct/enum variant.
    pub ident: Cow<'ast, Ident>,
    /// The direct descendants this field, as they appear in the definition.
    pub tys: HashSet<AType<'ast>>,
    /// All the other types that appear in this field. We need this to be a collection instead of just one because it may track _all_ types that are at or below the one defined for the field.
    pub indirect_tys: HashSet<AType<'ast>>,
}

impl<'ast> AField<'ast> {
    pub fn all_tys(&self) -> impl Iterator<Item = &'_ AType<'ast>> + '_ {
        self.tys.iter().chain(self.indirect_tys.iter())
    }
    /// Needed since all_tys cannot be an ExactSizeIterator, due to using .chain() internally.
    pub fn num_all_tys(&self) -> usize {
        self.tys.len() + self.indirect_tys.len()
    }
}

/// An "annotated type" of a field that corresponds to one of our lattice types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AType<'ast> {
    /// An index into the outer Nodes structure
    pub ident: &'ast Ident,
    /// The instantiation of generic arguments for this type. May correspond to the instantion
    /// defined in the field, or may be created as a result of propagating child relationships.
    pub instantiation: Vec<Cow<'ast, GenericArgument>>,
    /// The computed generic context, as collected from the parent. This can be used to determine
    /// which parts of `instantiation` are generic vs which are concrete.
    pub ctx: GenericContext<'ast>,
}

// We need to derive this and other traits manually, to ignore `ctx`
impl<'ast> Hash for AType<'ast> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
        self.instantiation.hash(state);
    }
}

fn convert_field<'ast>(
    nodes: &BaseNodes<'ast>,
    ctx: &GenericContext<'ast>,
    field: &'ast Field,
    index: usize,
) -> AField<'ast> {
    struct TypeVisitor<'ast, 'a> {
        /// The types we're looking for
        nodes: &'a BaseNodes<'ast>,
        /// All the collected types we've seen so far
        tys: HashSet<AType<'ast>>,
        /// The generic context of the node we're currently at
        ctx: &'a GenericContext<'ast>,
    }
    impl<'ast, 'a> Visit<'ast> for TypeVisitor<'ast, 'a> {
        fn visit_type_path(&mut self, ty: &'ast syn::TypePath) {
            // If it's just a single name in the path, it's highly likely it's part of the AST
            // we're building. Check for this.
            if ty.qself.is_none() && ty.path.segments.len() == 1 {
                let segment = ty.path.segments.first().unwrap();
                let ident = &segment.ident;
                if self.nodes.contains_key(ident) {
                    let instantiation =
                        if let PathArguments::AngleBracketed(args) = &segment.arguments {
                            args.args.iter().map(Cow::Borrowed).collect()
                        } else {
                            Vec::default()
                        };
                    let ctx = instantiation_collect_context(
                        self.ctx,
                        instantiation.iter().map(Deref::deref),
                    );
                    self.tys.insert(AType {
                        ident,
                        instantiation,
                        ctx,
                    });
                }
            }

            visit::visit_type_path(self, ty);
        }
    }

    let mut visitor = TypeVisitor {
        nodes,
        tys: HashSet::new(),
        ctx,
    };
    visitor.visit_field(field);

    let ident = field.ident.as_ref().map_or_else(
        || Cow::Owned(Ident::new(&format!("tmp_{index}"), field.ty.span())),
        Cow::Borrowed,
    );

    AField {
        ident,
        tys: visitor.tys,
        indirect_tys: Default::default(),
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
    ctx: &GenericContext<'ast>,
    fields: &'ast Fields,
) -> AVariant<'ast> {
    let named = matches!(fields, syn::Fields::Named(_));
    let unit = matches!(fields, syn::Fields::Unit);
    let fields = fields
        .iter()
        .enumerate()
        .map(|(index, field)| convert_field(nodes, ctx, field, index))
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

    pub fn ctx(&self) -> &GenericContext<'ast> {
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

    pub fn all_tys(&self) -> impl Iterator<Item = &'_ AType<'ast>> + '_ {
        self.fields().map(|field| field.all_tys()).flatten()
    }

    pub fn direct_tys(&self) -> impl Iterator<Item = &'_ AType<'ast>> + '_ {
        self.fields().map(|field| field.tys.iter()).flatten()
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
    let ctx = generics_collect_context(&item_struct.generics);
    let data = convert_variant(nodes, &item_struct.ident, &ctx, &item_struct.fields);

    AStruct {
        data,
        ctx,
        generics: &item_struct.generics,
    }
}

fn convert_enum<'ast>(nodes: &BaseNodes<'ast>, item_enum: &'ast ItemEnum) -> AEnum<'ast> {
    let ident = &item_enum.ident;
    let ctx = generics_collect_context(&item_enum.generics);
    let variants = item_enum
        .variants
        .iter()
        .map(|variant| convert_variant(nodes, &variant.ident, &ctx, &variant.fields))
        .collect();

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
                                tys: [AType {
                                    ident,
                                    instantiation: Default::default(),
                                }]
                                .into_iter()
                                .collect(),
                                indirect_tys: Default::default(),
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
