use syn::GenericArgument;
use syn::GenericParam;
use syn::Generics;
use syn::Ident;
use syn::Token;
use syn::WhereClause;
use syn::spanned::Spanned;
use syn::visit_mut;
use syn::visit_mut::VisitMut;

use crate::nodes::GenericContext;

/// Visitor that will append a suffix to all generic parameters in a set of idents
struct AddSuffix<'ast, 'suffix, 'hashset> {
    // Should be CamelCase
    suffix: &'suffix str,
    ctx: &'hashset GenericContext<'ast>,
}

impl<'ast, 'suffix, 'hashset> VisitMut for AddSuffix<'ast, 'suffix, 'hashset> {
    fn visit_lifetime_mut(&mut self, l: &mut syn::Lifetime) {
        if self.ctx.has_lifetime(&l.ident) {
            l.ident = Ident::new(
                &format!("{}_{}", l.ident, self.suffix.to_lowercase()),
                l.ident.span(),
            );
        }
        visit_mut::visit_lifetime_mut(self, l);
    }

    fn visit_type_param_mut(&mut self, t: &mut syn::TypeParam) {
        if self.ctx.has_type(&t.ident) {
            t.ident = Ident::new(&format!("{}{}", t.ident, self.suffix), t.ident.span());
        }
    }

    fn visit_type_path_mut(&mut self, t: &mut syn::TypePath) {
        if let Some(first) = t.path.segments.first_mut()
            && t.qself.is_none()
            && self.ctx.has_type(&first.ident)
        {
            first.ident = Ident::new(
                &format!("{}{}", first.ident, self.suffix),
                first.ident.span(),
            );
        }
        visit_mut::visit_type_path_mut(self, t);
    }

    fn visit_ident_mut(&mut self, i: &mut syn::Ident) {
        if self.ctx.has_const(i) {
            *i = Ident::new(&format!("{}_{}", i, self.suffix.to_uppercase()), i.span());
        }
        visit_mut::visit_ident_mut(self, i);
    }
}

#[derive(Copy, Clone)]
pub enum Behavior {
    /// Keep everything, regardless of if it's in the context,
    KeepAll,
    /// Only parameters present in the context should be returned
    OnlyCtx,
}

impl Behavior {
    fn keep_param<'ast>(&self, ctx: &GenericContext<'ast>, param: &GenericParam) -> bool {
        use Behavior::*;
        match self {
            KeepAll => return true,
            OnlyCtx => {}
        };

        match param {
            GenericParam::Lifetime(l) => ctx.has_lifetime(&l.lifetime.ident),
            GenericParam::Type(t) => ctx.has_type(&t.ident),
            GenericParam::Const(c) => ctx.has_const(&c.ident),
        }
    }
}

/// Adds a string to the idents of all of the given generic arguments present in `ctx`, returning the resulting generics.
/// If `remove` is true, removes all parameters that don't correspond to anything in `ctx`.
pub fn generics_add_suffix<'ast>(
    generics: &Generics,
    suffix: &str,
    ctx: &GenericContext<'ast>,
    behavior: Behavior,
) -> Generics {
    let mut generics = generics.clone();
    let mut v = AddSuffix { suffix, ctx };
    generics.params = generics
        .params
        .into_iter()
        .filter(|param| behavior.keep_param(ctx, param))
        .map(|mut param| {
            v.visit_generic_param_mut(&mut param);
            param
        })
        .collect();
    generics
}

/// Adds a string to all identifiers found in `ctx`
pub fn instantiation_add_suffix<'ast>(
    instantiation: impl Iterator<Item = &'ast GenericArgument> + 'ast,
    suffix: &'ast str,
    ctx: &'ast GenericContext<'ast>,
) -> impl Iterator<Item = GenericArgument> + 'ast {
    instantiation.map(|arg| {
        let mut arg = arg.clone();
        AddSuffix { suffix, ctx }.visit_generic_argument_mut(&mut arg);
        arg
    })
}

/// Merges two sets of generic parameters together
pub fn generics_merge(a: &Generics, b: &Generics) -> Generics {
    let params = a
        .params
        .clone()
        .into_iter()
        .chain(b.params.clone())
        .collect();
    let where_clause = match (a.where_clause.as_ref(), b.where_clause.as_ref()) {
        (None, None) => None,
        (Some(a_where), None) => Some(a_where.clone()),
        (None, Some(b_where)) => Some(b_where.clone()),
        (Some(a_where), Some(b_where)) => {
            let predicates = a_where
                .predicates
                .clone()
                .into_iter()
                .chain(b_where.predicates.clone())
                .collect();
            Some(WhereClause {
                where_token: <Token![where]>::default(),
                predicates,
            })
        }
    };

    Generics {
        lt_token: Some(<Token![<]>::default()),
        params,
        gt_token: Some(<Token![>]>::default()),
        where_clause,
    }
}
