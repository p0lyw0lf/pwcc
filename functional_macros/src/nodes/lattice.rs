use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Deref;

use syn::visit_mut;
use syn::visit_mut::VisitMut;
use syn::Expr;
use syn::GenericArgument;
use syn::GenericParam;
use syn::Ident;
use syn::Token;
use syn::Type;

use crate::nodes::instantiation_collect_context;
use crate::nodes::ANode;
use crate::nodes::ANodes;
use crate::nodes::AType;
use crate::nodes::GenericContext;

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
/// ```rust,ignore
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
/// ```rust,ignore
/// impl<A> Trait<LevelTwo<B>> for LevelOne<A> {
///     // ...
/// }
/// ```
///
/// So, we need to do something smarter. Given the definition in `LevelZero` substitutes `A` for
/// the parameter `B` in `LevelOne`, we need to replace all instantiations of `B` with `A` if we're
/// to move `LevelTwo` up to `LevelZero`. That's what this function does.
pub fn collapse_ty_edge<'ast>(
    a_ctx: &GenericContext<'ast>,
    ab: &AType<'ast>,
    b: &ANode<'ast>,
    bc: &AType<'ast>,
) -> AType<'ast> {
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
                let first = match rest.next() {
                    Some(first) => first,
                    None => return,
                };
                let substitution = match self.type_map.get(&first.ident) {
                    Some(s) => s,
                    None => return,
                };
                assert!(
                    path.qself.is_none(),
                    "unexpected qualified type in substituter"
                );
                if rest.len() > 0 {
                    // Best-effort; not 100% sure this will work all the time, may run into E0223:
                    // ambiguous associated type errors.
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
                    });
                } else {
                    *ty = (*substitution).clone();
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
    assert_eq!(
        b.generics().params.len(),
        ab.instantiation.len(),
        "generic instantiations for node {} and edge {} should have the same length. Do you have an error in your declaration?",
        b.ident(),
        ab.ident
    );
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
        .collect::<Vec<_>>();

    let ctx = instantiation_collect_context(a_ctx, ac_instantiation.iter().map(Deref::deref));
    AType {
        ident: bc.ident,
        instantiation: ac_instantiation,
        ctx,
    }
}

/// Wrapper type to let callers know that make_lattice has run on a graph.
#[derive(Debug, Default)]
pub(crate) struct Lattice<'ast>(pub ANodes<'ast>);

impl<'ast> Deref for Lattice<'ast> {
    type Target = ANodes<'ast>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Propagate children data to all nodes, recording, for every field, all of the
/// instantiations that can happen below it
/// TODO: this algorithm is handily O(v^2*e). It could probably be cut down to O(v^2) if we
/// properly kept track of what paths have been visited. But this is fine for now :P
pub(crate) fn make_lattice<'ast>(mut nodes: ANodes<'ast>) -> Lattice<'ast> {
    let mut changed = true;
    // By virtue of always iterating in the same order every time, we make an optimization: instead
    // of storing some sort of map from node name -> field name -> new types, we just store a list
    // of new types, iterating once to populate it, and then again to apply it.
    let mut new_types = Vec::<HashSet<AType<'ast>>>::new();
    while changed {
        changed = false;
        // Pass 3.1: Read in the next reachable set
        for a in nodes.values() {
            for a_field in a.fields() {
                let mut new_field_types = HashSet::new();
                for ab in a_field.all_tys() {
                    let b = nodes.get(ab.ident).unwrap();
                    for bc in b.all_tys() {
                        new_field_types.insert(collapse_ty_edge(a.ctx(), ab, b, bc));
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
            let old_num_types = field.num_all_tys();
            field.indirect_tys.extend(new_field_types);
            let new_num_types = field.num_all_tys();
            if old_num_types != new_num_types {
                changed = true;
            }
        }

        // new_types should be completely clear at this point
        assert_eq!(
            new_types.len(),
            0,
            "new types should be complerely clear at this point"
        );
    }

    // Pass 4: Check all nodes for coherence.
    // TODO: it's probably good to do that, but also not strictly necessary, since the compiler
    // will reject exactly what it needs to anyways?

    Lattice(nodes)
}
