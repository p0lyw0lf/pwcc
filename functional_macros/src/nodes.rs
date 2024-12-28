use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;

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

pub mod lattice;
pub mod scc;

/// Maps ident string to original definition
#[derive(Default)]
struct BaseNodes<'ast>(HashMap<String, BaseNode<'ast>>);

impl<'ast> Visit<'ast> for BaseNodes<'ast> {
    fn visit_item_struct(&mut self, node: &'ast ItemStruct) {
        self.0
            .insert(node.ident.to_string(), BaseNode::Struct(node));
        visit::visit_item_struct(self, node);
    }
    fn visit_item_enum(&mut self, node: &'ast ItemEnum) {
        self.0.insert(node.ident.to_string(), BaseNode::Enum(node));
        visit::visit_item_enum(self, node);
    }
}

/// All of the identifiers used inside a generic context. E.g., if we have a
/// struct definition like:
///
/// ```rust
/// enum Result<T, E> {
///     Ok(T),
///     Err(E),
/// }
/// ```
///
/// Then the GenericContext inside the `Result` body is `<T, E>`, and the GenericContext used by
/// the `Ok` variant is `T`.
#[derive(Debug, Default, PartialEq)]
pub(crate) struct GenericContext<'ast> {
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
pub(crate) struct AField<'ast> {
    /// Corresponds to the declared ident if in a named struct/enum variant, otherwise corresponds to an anonymous ident if in an unnamed struct/enum variant.
    pub ident: Cow<'ast, Ident>,
    /// All the types that appear in this field. We need this to be a collection instead of just one because it may track _all_ types that are at or below the one defined for the field.
    pub types: HashSet<AType<'ast>>,
}

/// An "annotated type" of a field that corresponds to one of our lattice types.
#[derive(Debug, Hash, PartialEq, Eq)]
pub(crate) struct AType<'ast> {
    /// An index into the outer Nodes structure
    pub key: String,
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
                let ident = segment.ident.to_string();
                if self.nodes.0.contains_key(&ident) {
                    self.types.insert(AType {
                        key: ident,
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
pub(crate) struct AVariant<'ast> {
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
pub(crate) struct AStruct<'ast> {
    pub data: AVariant<'ast>,
    /// The generic context defined by this struct
    pub ctx: GenericContext<'ast>,
    /// The exact order of generic arguments
    pub generics: &'ast Generics,
}

/// An "annotated enum", representing a standalone enum
#[derive(Debug)]
pub(crate) struct AEnum<'ast> {
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
pub(crate) enum ANode<'ast> {
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

#[derive(Default, Debug)]
pub(crate) struct ANodes<'ast>(pub BTreeMap<String, ANode<'ast>>);

pub(crate) fn make_nodes<'ast>(m: &'ast syn::ItemMod) -> ANodes<'ast> {
    // Pass 1: Read in most basic data by visiting all structs/enums
    let mut base = BaseNodes::default();
    base.visit_item_mod(m);

    // Pass 2: Annotate nodes with generic context data for their direct children.
    let nodes = ANodes(
        base.0
            .iter()
            .map(|(key, node)| {
                (
                    key.clone(),
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
