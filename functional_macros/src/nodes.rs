use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Deref;

use syn::spanned::Spanned;
use syn::visit;
use syn::visit::Visit;
use syn::visit_mut;
use syn::visit_mut::VisitMut;
use syn::Expr;
use syn::Field;
use syn::Fields;
use syn::GenericArgument;
use syn::GenericParam;
use syn::Generics;
use syn::Ident;
use syn::ItemEnum;
use syn::ItemStruct;
use syn::PathArguments;
use syn::Token;
use syn::Type;

enum BaseNode<'ast> {
    Struct(&'ast ItemStruct),
    Enum(&'ast ItemEnum),
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

/// Collects all the idents found in a Generics node
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

/// Collects all the idents found in an instantiation, given a parent `ctx`
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
            let first = t.path.segments.first().expect("empty type path");
            if t.qself.is_none() && self.parent_ctx.has_type(&first.ident) {
                self.ctx.types.insert(&first.ident);
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
    /// All the lattice types that appear in this field. We need this to be a Vec because it tracks
    /// _all_ types that are at or below the one defined for the field.
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
        types: &'a mut HashSet<AType<'ast>>,
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

    let mut types = HashSet::new();
    let mut visitor = TypeVisitor {
        nodes,
        types: &mut types,
    };
    visitor.visit_field(field);

    let ident = field.ident.as_ref().map_or_else(
        || Cow::Owned(Ident::new(&format!("tmp_{index}"), field.ty.span())),
        Cow::Borrowed,
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

/// Given a path like A -> B -> C, collapse it into A -> C
///
/// The edges A -> B and B -> C are represented by AType items, and we need the
/// intermediate ANode item B to find out how to translate the idents in B -> C.
///
/// REQUIRIES: `b` is actually the node pointed to by `ab`, and `bc` is actually an outgoing edge
/// of `b`
///
/// Say you have an AST like so:
///
/// ```rust
/// struct LevelZero<A> {
///     one: LevelOne<A>,
/// }
/// struct LevelOne<B> {
///     two: LevelTwo<B>,
/// }
/// struct LevelTwo<C>(C);
/// ```
///
/// Then, when you want to generate an implementation over `LevelTwo` for `LevelZero`, you'll need
/// to substitute in the correct type parameter. But! If we were do this naively, i.e. just reuse
/// the definition inside `LevelOne`, we'd end up with something like:
///
/// ```rust
/// impl<A> Trait<LevelTwo<B>> for LevelOne<A> {
///     // ...
/// }
/// ```
///
/// So, we need to do something smarter. Given the definition in `LevelZero` substitutes `A` for
/// the parameter `B` in `LevelOne`, we need to replace all instantiations of `B` with `A` if we're
/// to move `LevelTwo` up to `LevelZero`. That's what this function does.
fn collapse_ty_edge<'ast>(ab: &AType<'ast>, b: &ANode<'ast>, bc: &AType<'ast>) -> AType<'ast> {
    /// There's considerations like:
    /// one: LevelOne<A::Mapped>, two: LevelTwo<B::Mapped> needs to turn into
    /// LevelOne<(A::Mapped)::Mapped>. Similarly, we could have generic expressions instead of just
    /// raw generics (tho I'm not sure that's 100% supported. anyways). So, we need to operate at a
    /// higher level than just ident -> ident substitutions, we need ident -> type and ident ->
    /// expr substitutions too!
    struct Substituter<'ast> {
        /// Maps the lifetime used in b to the lifetime used in a
        lifetime_map: HashMap<&'ast Ident, &'ast Ident>,
        /// Maps the generic type variable used in b to the type substitution as passed in from a
        type_map: HashMap<&'ast Ident, &'ast Type>,
        /// Maps the generic constant as used in b to the constant expression as passed in from a
        constant_map: HashMap<&'ast Ident, &'ast Expr>,
    }
    impl<'ast> VisitMut for Substituter<'ast> {
        fn visit_type_mut(&mut self, ty: &mut syn::Type) {
            // Visit inner parts of path first. If we visited afterwards, we might get confused and
            // replace things inside of a part we've already replaced.
            visit_mut::visit_type_mut(self, ty);
            if let Type::Path(path) = ty {
                // If the first type in the path is in the generic context, we should substitute it.
                let mut rest = path.path.segments.iter();
                let first = rest.next().expect("empty type path");
                if let Some(substitution) = self.type_map.get(&first.ident) {
                    assert!(path.qself.is_none());
                    *ty = Type::Path(syn::TypePath {
                        qself: Some(syn::QSelf {
                            lt_token: <Token![<]>::default(),
                            ty: Box::new((*substitution).clone()),
                            position: 0,
                            as_token: None,
                            gt_token: <Token![>]>::default(),
                        }),
                        path: syn::Path {
                            leading_colon: Some(<Token![::]>::default()),
                            segments: rest.map(Clone::clone).collect(),
                        },
                    })
                }
            }
        }

        fn visit_lifetime_mut(&mut self, i: &mut syn::Lifetime) {
            visit_mut::visit_lifetime_mut(self, i);
            if let Some(substitution) = self.lifetime_map.get(&i.ident) {
                i.ident = (*substitution).clone();
            }
        }

        fn visit_expr_mut(&mut self, e: &mut syn::Expr) {
            visit_mut::visit_expr_mut(self, e);
            if let Expr::Path(path) = e {
                if path.qself.is_none() && path.path.segments.len() == 1 {
                    let ident = path.path.get_ident().unwrap();
                    if let Some(substitution) = self.constant_map.get(&ident) {
                        *e = (*substitution).clone();
                    }
                }
            }
        }
    }

    let mut lifetime_map = HashMap::new();
    let mut type_map = HashMap::new();
    let mut constant_map = HashMap::new();

    // Make substituter maps by iterating over the definition and instantiation at the same time
    assert_eq!(b.generics().params.len(), ab.instantiation.len());
    for (key, value) in b.generics().params.iter().zip(ab.instantiation.iter()) {
        match key {
            GenericParam::Lifetime(key) => {
                if let GenericArgument::Lifetime(value) = value.deref() {
                    lifetime_map.insert(&key.lifetime.ident, &value.ident);
                } else {
                    panic!("got unexpected arg {:?} trying to match against {} in the definition of {}", value, key.lifetime, b.ident());
                }
            }
            GenericParam::Type(key) => {
                if let GenericArgument::Type(value) = value.deref() {
                    type_map.insert(&key.ident, value);
                } else {
                    panic!("got unexpected arg {:?} trying to match against {} in the definition of {}", value, key.ident, b.ident());
                }
            }
            GenericParam::Const(key) => {
                if let GenericArgument::Const(value) = value.deref() {
                    constant_map.insert(&key.ident, value);
                } else {
                    panic!("got unexpected arg {:?} trying to match against {} in the definition of {}", value, key.ident, b.ident());
                }
            }
        }
    }

    let mut substituter = Substituter {
        lifetime_map,
        type_map,
        constant_map,
    };

    let ac_instantiation = bc
        .instantiation
        .iter()
        .map(|generic_arg| match generic_arg.clone().into_owned() {
            GenericArgument::Lifetime(mut l) => Cow::Owned(GenericArgument::Lifetime({
                substituter.visit_lifetime_mut(&mut l);
                l
            })),
            GenericArgument::Type(mut t) => Cow::Owned(GenericArgument::Type({
                substituter.visit_type_mut(&mut t);
                t
            })),
            GenericArgument::Const(mut e) => Cow::Owned(GenericArgument::Const({
                substituter.visit_expr_mut(&mut e);
                e
            })),
            other => panic!(
                "got unexpected generic argument {:?} while parsing item{}",
                other,
                b.ident()
            ),
        })
        .collect();

    AType {
        key: bc.key.clone(),
        instantiation: ac_instantiation,
    }
}

#[derive(Default, Debug)]
pub(crate) struct ANodes<'ast>(pub BTreeMap<String, ANode<'ast>>);

pub(crate) fn make_nodes<'ast>(m: &'ast syn::ItemMod) -> ANodes<'ast> {
    // Pass 1: Read in most basic data by visiting all structs/enums
    let mut base = BaseNodes::default();
    base.visit_item_mod(m);

    // Pass 2: Annotate nodes with generic context data for their direct children.
    let mut nodes = ANodes(
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
    // TODO: this algorithm is handily O(v^2*e). It could probably be cut down to O(v^2) if we
    // properly kept track of what paths have been visited. But this is fine for now :P
    let mut changed = true;
    // By virtue of always iterating in the same order every time, we make an optimization: instead
    // of storing some sort of map from node name -> field name -> new types, we just store a list
    // of new types, iterating once to populate it, and then again to apply it.
    let mut new_types = Vec::<HashSet<AType<'ast>>>::new();
    while changed {
        changed = false;
        // Pass 3.1: Read in the next reachable set
        for a in nodes.0.values() {
            for a_field in a.fields() {
                let mut new_field_types = HashSet::new();
                for ab in a_field.types.iter() {
                    let b = nodes.0.get(&ab.key).unwrap();
                    for bc in b.fields().map(|field| field.types.iter()).flatten() {
                        new_field_types.insert(collapse_ty_edge(ab, b, bc));
                    }
                }
                new_types.push(new_field_types);
            }
        }
        // Pass 3.2: Extend graph by the reachability step
        for (field, new_field_types) in nodes
            .0
            .values_mut()
            .map(|a| a.fields_mut())
            .flatten()
            .zip(new_types.drain(..))
        {
            let old_num_types = field.types.len();
            field.types.extend(new_field_types);
            let new_num_types = field.types.len();
            if old_num_types != new_num_types {
                changed = true;
            }
        }

        // new_types should be completely clear at this point
        assert_eq!(new_types.len(), 0);
    }

    // Pass 4: Check all nodes for coherence.
    // TODO: it's probably good to do that, but also not strictly necessary, since the compiler
    // will reject exactly what it needs to anyways?

    nodes
}
