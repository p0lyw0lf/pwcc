use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;

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
fn generics_collect_context<'ast>(generics: &'ast Generics) -> GenericContext<'ast> {
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
#[derive(Debug)]
pub(crate) struct AType<'ast> {
    /// An index into the outer Nodes structure
    pub key: String,
    /// The instantiation of generic arguments for this type. May correspond to the instantion
    /// defined in the field, or may be created as a result of propagating child relationships.
    pub instantiation: Vec<GenericArgument>,
    /// All generics from the surrounding context that are used by this type.
    /// INVARIANT: can be derived exactly from `instantiation`
    pub ctx: GenericContext<'ast>,
}

impl<'ast> std::hash::Hash for AType<'ast> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.key.hash(state);
        self.instantiation.hash(state);
    }
}

impl<'ast> std::cmp::PartialEq for AType<'ast> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key && self.instantiation == other.instantiation
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
        types: &'a mut Vec<AType<'ast>>,
        /// The parent generic context
        parent_ctx: &'a GenericContext<'ast>,
        /// The current context we're trying to add to. Keeps track of what parts of the
        /// parrent_ctx are used in the instantiation of the current type.
        current_ctx: GenericContext<'ast>,
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
                            args.args.iter().map(Clone::clone).collect()
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
                let ident = &segment.ident;
                if self.parent_ctx.types.contains(ident) {
                    self.current_ctx.types.insert(ident);
                }
            }

            visit::visit_type_path(self, ty);
        }

        fn visit_lifetime(&mut self, i: &'ast syn::Lifetime) {
            let ident = &i.ident;
            if self.parent_ctx.has_lifetime(ident) {
                self.current_ctx.lifetimes.insert(ident);
            }
            visit::visit_lifetime(self, i);
        }

        // Unfortunately, there's not really a better way to check for consts...
        // TODO: could maybe make it more efficient by only starting to look whenever we're in an Expr context?
        fn visit_ident(&mut self, i: &'ast proc_macro2::Ident) {
            if self.parent_ctx.consts.contains(i) {
                self.current_ctx.consts.insert(i);
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
    ctx: &GenericContext<'ast>,
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

/// Checks a struct for coherence.
///
/// Concretely, what this means is, if we have a struct like so:
///
/// ```rust
/// struct Both<A, B> {
///     a: Exp<A>,
///     b: Exp<B>,
/// }
/// ```
///
/// Then it's not possible for us to, without outside context, emit a useful implementations
/// mapping over `Exp` both `Both`. If we were to try, they might look something like:
///
/// ```rust
/// impl<A, B> Trait<Exp<A>> for Both<A, B> {
///     // ...
/// }
/// impl<A, B> Trait<Exp<B>> for Both<A, B> {
///    // ...
/// }
/// ```
///
/// This obviously runs afoul of coherence rules; we could have A == B after all! We don't have
/// specialization and we don't have negative trait bounds, so there's no getting out of this
/// pickle. If the user really wants a struct like this, they'll have to implement it manually with
/// the `#[specialize]` macro.
fn check_coherence_struct<'ast>(s: &AStruct<'ast>) {
    let mut found = HashMap::new();
    check_coherence_variant(&mut found, &s.data);
}

/// See `check_coherence_struct`
fn check_coherence_enum<'ast>(e: &AEnum<'ast>) {
    let mut found = HashMap::new();
    for variant in e.variants.iter() {
        check_coherence_variant(&mut found, variant);
    }
}

/// Checks if a given variant coheres with the contexts for all the previous variants
fn check_coherence_variant<'ast, 'parent>(
    previous: &mut HashMap<&'parent str, &'parent GenericContext<'ast>>,
    variant: &'parent AVariant<'ast>,
) {
    for field in variant.fields.iter() {
        for ty in field.types.iter() {
            check_coherence_type(previous, ty);
        }
    }
}

/// Checks if a given type coheres with the contexts for all the previous types
fn check_coherence_type<'ast, 'parent>(
    previous: &mut HashMap<&'parent str, &'parent GenericContext<'ast>>,
    ty: &'parent AType<'ast>,
) {
    match previous.entry(&ty.key) {
        Entry::Occupied(e) => {
            // The automatically-derived PartialEq implementation works here
            if e.get() != &&ty.ctx {
                // TODO: make this compiler error better
                panic!(
                    "{} with context {:?} won't cohere with previously found context {:?}",
                    ty.key,
                    ty.ctx,
                    e.get()
                );
            }
        }
        Entry::Vacant(e) => {
            e.insert(&ty.ctx);
        }
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
                if let GenericArgument::Lifetime(value) = value {
                    lifetime_map.insert(&key.lifetime.ident, &value.ident);
                } else {
                    panic!("got unexpected arg {:?} trying to match against {} in the definition of {}", value, key.lifetime, b.ident());
                }
            }
            GenericParam::Type(key) => {
                if let GenericArgument::Type(value) = value {
                    type_map.insert(&key.ident, value);
                } else {
                    panic!("got unexpected arg {:?} trying to match against {} in the definition of {}", value, key.ident, b.ident());
                }
            }
            GenericParam::Const(key) => {
                if let GenericArgument::Const(value) = value {
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
        .map(|generic_arg| match generic_arg {
            GenericArgument::Lifetime(l) => GenericArgument::Lifetime({
                let mut l = l.clone();
                substituter.visit_lifetime_mut(&mut l);
                l
            }),
            GenericArgument::Type(t) => GenericArgument::Type({
                let mut t = t.clone();
                substituter.visit_type_mut(&mut t);
                t
            }),
            GenericArgument::Const(e) => GenericArgument::Const({
                let mut e = e.clone();
                substituter.visit_expr_mut(&mut e);
                e
            }),
            other => panic!(
                "got unexpected generic argument {:?} while parsing item{}",
                other,
                b.ident()
            ),
        })
        .collect();

    // We need this to check for coherence later. TODO: actually implement the transform for it
    let ac_ctx = GenericContext::default();

    AType {
        key: bc.key.clone(),
        instantiation: ac_instantiation,
        ctx: ac_ctx,
    }
}

#[derive(Default, Debug)]
pub(crate) struct ANodes<'ast>(pub HashMap<String, ANode<'ast>>);

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

    // Pass 3: propagate children data to all nodes, recording, for every field, all of the
    // instantiations that can happen below it
    // TODO: this seems pretty hard. Glad I at least wrote a comment about it lol

    // Pass 3.1: Check all nodes for coherence.
    for node in nodes.0.values() {
        match node {
            ANode::Struct(s) => check_coherence_struct(s),
            ANode::Enum(e) => check_coherence_enum(e),
        }
    }

    nodes
}
