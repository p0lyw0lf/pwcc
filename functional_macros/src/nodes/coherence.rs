//! This crate is in charge of pruning an ANodes graph to only have outgoing edges that make sense
//! for generating `impl`s for.
//!
//! The doc comment in `crate::nodes::scc` gives an example of one type of edge we might want to
//! remove, and the order in which we must traverse nodes in order to do so (reverse topologically
//! sorted among strong connected components). However, there is one more type of edge we'll have
//! to remove: edges that would lead to incoherent implementations.
//!
//! Consider the following example:
//!
//! ```rust,ignore
//! struct A<T, U> {
//!     bt: B<T>,
//!     bu: B<U>,
//! }
//! struct B<T>(T);
//! trait Trait<T> {}
//! ```
//!
//! Then, the following implementations will be incoherent, because we're trying to generate
//! multiple trait implementations for the "same" base type:
//!
//! ```rust,ignore
//! impl<T, U> Trait<B<T>> for A<T, U> {}
//! impl<T, U> Trait<B<U>> for A<T, U> {}
//! ```
//!
//! The check for this is simple: If, for a given outgoing edge, there exists another outgoing edge
//! with a different generic context, we should remove that edge.
//!
//! Because we're already going thru all nodes to remove edges based on SCC connectivity, we should
//! remove these edges before that pass, or, during that pass. To keep the invariant imposed by
//! `Lattice`, we'll do it at the same time.

use std::collections::HashSet;
use std::ops::Deref;

use syn::Ident;

use crate::nodes::instantiation_collect_context;
use crate::nodes::lattice::collapse_ty_edge;
use crate::nodes::scc::StronglyConnectedComponents;
use crate::nodes::scc::find_sccs;
use crate::nodes::ANode;
use crate::nodes::ANodes;
use crate::nodes::AType;
use crate::nodes::GenericContext;
use crate::nodes::lattice::Lattice;

/// The first order of business is to run the topological sort to find the ordering we do the
/// filtering in. We'll use the [Depth-first search](https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search) method.
/// ENSURES: \foreach i in \return. i \in sccs.nodes
fn topological_sort<'ast>(sccs: &StronglyConnectedComponents<'ast>) -> Vec<&'ast Ident> {
    #[derive(Default)]
    struct State<'ast> {
        /// The list that will contain the sorted nodes
        order: Vec<&'ast Ident>,
        /// The "permamently marked" nodes in the algorithm. Here, a node is represented by the
        /// strongly connected component index.
        permanent_marks: HashSet<usize>,
        /// The "temporarily marked" nodes in the algorithm. We should not need these, because the
        /// links among the strongly connected components should always form a DAG, but it's good
        /// to have the cycle check anyways.
        temporary_marks: HashSet<usize>,
    }

    impl<'ast> State<'ast> {
        fn visit(&mut self, sccs: &StronglyConnectedComponents<'ast>, node: &ANode<'ast>) {
            let ident = node.ident();
            let index = sccs.node_to_index[ident];
            if self.permanent_marks.contains(&index) {
                return;
            }
            if self.temporary_marks.contains(&index) {
                panic!("cycle detected while running topological sort! this should not happen");
            }

            self.temporary_marks.insert(index);

            // Visit all other components that are connected to this one
            for next_node in sccs.index_to_scc[index]
                .iter()
                .map(|ident| sccs.nodes[ident].all_tys())
                .flatten()
                .map(|ty| &sccs.nodes[ty.ident])
            {
                let next_index = sccs.node_to_index[next_node.ident()];
                if next_index != index {
                    self.visit(sccs, &next_node);
                }
            }

            self.permanent_marks.insert(index);

            // Add all the nodes in the component to the order
            self.order.extend(sccs.index_to_scc[index].iter());
        }
    }

    let mut state = State::default();
    for node in sccs.nodes.values() {
        state.visit(sccs, node);
    }

    state.order
}

pub fn filter_coherent<'ast>(lattice: Lattice<'ast>) -> ANodes<'ast> {
    let sccs = find_sccs(lattice);
    let order = topological_sort(&sccs);
    let mut nodes = sccs.nodes.0;

    for ident in order.iter() {
        let mut bad_edges = HashSet::<AType<'ast>>::new();
        // 1. Remove all outgoing edges where there are multiple different generic contexts
        let node = nodes.0.get(ident).unwrap();
        for other_ident in order.iter() {
            // Clone is perhaps expensive here, but necessary, because otherwise we may be left
            // with stale references to edges we want to remove
            let edges_with_other_ident = node
                .all_tys()
                .filter(|ty| ty.ident == *other_ident)
                .map(Clone::clone)
                .collect::<HashSet<_>>();
            if edges_with_other_ident.len() > 1 {
                // println!("Node {ident}: filtering {other_ident} due to multiple conflicting generic contexts");
                bad_edges.extend(edges_with_other_ident);
            }
        }

        let node = nodes.0.get_mut(ident).unwrap();
        for field in node.fields_mut() {
            field.tys.retain(|ty| !bad_edges.contains(&ty));
            field.indirect_tys.retain(|ty| !bad_edges.contains(&ty));
        }

        bad_edges.clear();

        // 2. Remove all outgoing edges where there's other edges w/ overlapping generic contexts
        //    that cannot be transformed.
        // First, let's cache all the generic contexts that result from the types
        #[derive(Debug)]
        struct ATypeWithContext<'ast, 'local> {
            ty: AType<'ast>,
            ctx: GenericContext<'local>,
        }
        let node = nodes.0.get(ident).unwrap();
        let tys = node
            .all_tys()
            .collect::<HashSet<_>>()
            .into_iter()
            .map(|ty| ATypeWithContext {
                ty: ty.clone(),
                ctx: instantiation_collect_context(node.ctx(), ty.instantiation.iter().map(Deref::deref)),
            })
            .collect::<Vec<_>>();
        let direct_tys = node.direct_tys().collect::<HashSet<_>>();
        // Then, we'll look for pairs of edges where the contexts intersect, and the latter edge is
        // a direct edge.
        for edge_b in tys.iter() {
            for edge_c in tys
                .iter()
                .filter(|edge_c| direct_tys.contains(&edge_c.ty) && edge_b.ty != edge_c.ty && edge_b.ctx.intersects(&edge_c.ctx))
            {
                let node_c = &nodes[edge_c.ty.ident];
                // Look for an edge c -> b. If we can't find it, then a -> b is a bad edge.
                if !node_c
                    .all_tys()
                    .map(|ty| collapse_ty_edge(&edge_c.ty, node_c, ty))
                    .any(|ty| ty == edge_b.ty)
                {
                    let edge_b_ident = edge_b.ty.ident;
                    let edge_c_ident = edge_c.ty.ident;
                    // println!("Node {ident}: filtering {edge_b_ident} due to {edge_c_ident} not being able to be transformed by it");
                    bad_edges.insert(edge_b.ty.clone());
                }
            }
        }

        let node = nodes.0.get_mut(ident).unwrap();
        for field in node.fields_mut() {
            field.tys.retain(|ty| !bad_edges.contains(&ty));
            field.indirect_tys.retain(|ty| !bad_edges.contains(&ty));
        }
    }

    nodes
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::nodes::lattice::make_lattice;
    use crate::nodes::test::run_test;

    /// edges[i] contains a list of all other indicies of vertices for outgoing edges
    /// expected_edges[i] is what we expect the edge list to look like after filtering
    fn coherence_test(edges: &[&[usize]], expected_edges: &[&[usize]]) {
        assert_eq!(edges.len(), expected_edges.len());

        run_test(edges, |nodes, labels| {
            let nodes = make_lattice(nodes);
            let nodes = filter_coherent(nodes);

            let actual_edges = labels
                .iter()
                .map(|label| {
                    let node = &nodes[label];
                    let edges = node
                        .all_tys()
                        .collect::<HashSet<_>>()
                        .into_iter()
                        .map(|ty| labels.iter().position(|label| label == ty.ident).unwrap())
                        .collect::<Vec<_>>();
                    edges
                })
                .collect::<Vec<_>>();

            assert_eq!(expected_edges, actual_edges);
        });
    }

    #[test]
    fn simple() {
        // This is the example I reference everywhere
        coherence_test(&[&[1, 2], &[2], &[]], &[&[2], &[2], &[]]);
    }
}
