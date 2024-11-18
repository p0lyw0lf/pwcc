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

impl<'ast> BaseNode<'ast> {
    fn ident(&self) -> &'ast Ident {
        match self {
            BaseNode::Struct(s) => &s.ident,
            BaseNode::Enum(e) => &e.ident,
        }
    }

    fn generics(&self) -> &'ast Generics {
        match self {
            BaseNode::Struct(s) => &s.generics,
            BaseNode::Enum(e) => &e.generics,
        }
    }
}

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
pub(crate) struct GenericContext {
    pub lifetimes: HashSet<String>,
    pub types: HashSet<String>,
    pub consts: HashSet<String>,
}

/// Collects all the idents found in a Generics node
fn generics_collect_context(generics: &Generics) -> GenericContext {
    let mut out = GenericContext::default();

    for param in generics.params.iter() {
        match param {
            GenericParam::Lifetime(lp) => out.lifetimes.insert(lp.lifetime.ident.to_string()),
            GenericParam::Type(tp) => out.types.insert(tp.ident.to_string()),
            GenericParam::Const(cp) => out.consts.insert(cp.ident.to_string()),
        };
    }

    out
}

/// An "annotated field" in a struct/enum annotated with the requisite data to do introspection on.
#[derive(Debug)]
pub(crate) struct AField<'ast> {
    /// Corresponds to the declared ident if in a named struct/enum variant, otherwise corresponds to an anonymous ident if in an unnamed struct/enum variant.
    pub ident: Ident,
    /// All the lattice types that appear in this field. We need this to be a Vec because it tracks
    /// _all_ types that are at or below the one defined for the field.
    pub types: Vec<AType<'ast>>,
}

/// An "annotated type" of a field that corresponds to one of our lattice types.
#[derive(Debug, PartialEq)]
pub(crate) struct AType<'ast> {
    /// An index into the outer Nodes structure
    pub key: String,
    /// The instantiation of generic arguments for this type
    pub instantiation: Vec<&'ast GenericArgument>,
    /// All generics from the surrounding context that are used by this type
    pub ctx: GenericContext,
}

fn convert_field<'ast>(
    nodes: &BaseNodes<'ast>,
    ctx: &GenericContext,
    field: &'ast Field,
    index: usize,
) -> AField<'ast> {
    struct TypeVisitor<'ast, 'a> {
        /// The types we're looking for
        nodes: &'a BaseNodes<'ast>,
        /// All the collected types we've seen so far
        types: &'a mut Vec<AType<'ast>>,
        /// The parent generic context
        parent_ctx: &'a GenericContext,
        /// The current context we're trying to add to. Keeps track of what parts of the
        /// parrent_ctx are used in the instantiation of the current type.
        current_ctx: GenericContext,
    }
    impl<'ast, 'a> Visit<'ast> for TypeVisitor<'ast, 'a> {
        fn visit_type_path(&mut self, ty: &'ast syn::TypePath) {
            // If it's just a single name in the path, it's highly likely it's part of the AST
            // we're building. Check for this.
            if ty.qself.is_none() && ty.path.segments.len() == 1 {
                let segment = ty.path.segments.first().unwrap();
                let ident = segment.ident.to_string();
                if self.nodes.0.contains_key(&ident) {
                    // We found a type! put ourselves into a new context and recur

                    let old_ctx =
                        std::mem::replace(&mut self.current_ctx, GenericContext::default());

                    visit::visit_type_path(self, ty);

                    let ctx = std::mem::replace(&mut self.current_ctx, old_ctx);

                    self.types.push(AType {
                        key: ident,
                        instantiation: if let PathArguments::AngleBracketed(args) =
                            &segment.arguments
                        {
                            args.args.iter().collect()
                        } else {
                            Vec::default()
                        },
                        ctx,
                    });

                    return;
                }
            }

            // Otherwise, check if the first type in the segment is in the generic context. If it
            // is, record that
            if let Some(segment) = ty.path.segments.first() {
                let ident = segment.ident.to_string();
                if self.parent_ctx.types.contains(&ident) {
                    self.current_ctx.types.insert(ident);
                }
            }

            visit::visit_type_path(self, ty);
        }

        fn visit_lifetime(&mut self, i: &'ast syn::Lifetime) {
            let ident = i.ident.to_string();
            if self.parent_ctx.lifetimes.contains(&ident) {
                self.current_ctx.lifetimes.insert(ident);
            }
            visit::visit_lifetime(self, i);
        }

        // Unfortunately, there's not really a better way to check for consts...
        // TODO: could maybe make it more efficient by only starting to look whenever we're in an Expr context?
        fn visit_ident(&mut self, i: &'ast proc_macro2::Ident) {
            let ident = i.to_string();
            if self.parent_ctx.consts.contains(&ident) {
                self.current_ctx.consts.insert(ident);
            }
            visit::visit_ident(self, i);
        }
    }

    let mut types = Vec::new();
    let mut visitor = TypeVisitor {
        nodes,
        types: &mut types,
        parent_ctx: ctx,
        current_ctx: GenericContext::default(),
    };
    visitor.visit_field(field);

    let ident = field.ident.as_ref().map_or_else(
        || Ident::new(&format!("tmp_{index}"), field.ty.span()),
        Clone::clone,
    );

    AField { ident, types }
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
    ctx: &GenericContext,
    fields: &'ast Fields,
) -> AVariant<'ast> {
    let named = matches!(fields, syn::Fields::Named(_));
    let unit = matches!(fields, syn::Fields::Unit);
    let fields = fields
        .iter()
        .enumerate()
        .map(|(index, field)| convert_field(nodes, &ctx, field, index))
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
    pub ctx: GenericContext,
}

/// An "annotated enum", representing a standalone enum
#[derive(Debug)]
pub(crate) struct AEnum<'ast> {
    /// The original Ident token when the enum was defined
    pub ident: &'ast Ident,
    /// All of the variants in the enum
    pub variants: Vec<AVariant<'ast>>,
    /// The generic context defined by this enum
    pub ctx: GenericContext,
}

#[derive(Debug)]
pub(crate) enum ANode<'ast> {
    Struct(AStruct<'ast>),
    Enum(AEnum<'ast>),
}

fn convert_struct<'nodes, 'ast>(
    nodes: &'nodes BaseNodes<'ast>,
    item_struct: &'ast ItemStruct,
) -> AStruct<'ast> {
    let ctx = generics_collect_context(&item_struct.generics);
    let data = convert_variant(nodes, &item_struct.ident, &ctx, &item_struct.fields);

    AStruct { data, ctx }
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
    }
}

#[derive(Default, Debug)]
pub(crate) struct ANodes<'ast>(pub HashMap<String, ANode<'ast>>);

pub(crate) fn make_nodes<'ast>(m: &'ast syn::ItemMod) -> ANodes<'ast> {
    // Pass 1: read in most basic data by visiting all structs/enums
    let mut base = BaseNodes::default();
    base.visit_item_mod(m);

    // Pass 2: annotate nodes with generic context data for their direct children
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

    // Pass 3: propagate children data to all nodes, recording, for every field, all of the
    // instantiations that can happen below it
    // TODO: this seems pretty hard. Glad I at least wrote a comment about it lol

    nodes
}
