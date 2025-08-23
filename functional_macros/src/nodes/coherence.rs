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
//! To check this for all cases (like `B<i32>` and `B<U>`, or `C<i32, T>` and `C<T, i32>`), we have
//! to check the instantiations and apply the same logic Rust uses: concrete types must not appear
//! in the same location as generic types, and if generic types appear in the same location, they
//! must be the same.
//!
//! Because we're already going thru all nodes to remove edges based on SCC connectivity, we should
//! remove these edges before that pass, or, during that pass. To keep the invariant imposed by
//! `Lattice`, we'll do it at the same time.

use std::collections::HashSet;

use syn::Ident;

use crate::nodes::ANode;
use crate::nodes::ANodes;
use crate::nodes::AType;
use crate::nodes::lattice::Lattice;
use crate::nodes::lattice::collapse_ty_edge;
use crate::nodes::scc::StronglyConnectedComponents;
use crate::nodes::scc::find_sccs;

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
                .flat_map(|ident| sccs.nodes[ident].all_tys())
                .map(|ty| &sccs.nodes[ty.ident])
            {
                let next_index = sccs.node_to_index[next_node.ident()];
                if next_index != index {
                    self.visit(sccs, next_node);
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
        'other_ident: for other_ident in order.iter() {
            let edges_with_other_ident = node
                .all_tys()
                .filter(|ty| ty.ident == *other_ident)
                .map(Clone::clone)
                .collect::<HashSet<_>>();
            'next_edge: for edge_b in edges_with_other_ident.iter() {
                if edge_b.instantiation.is_empty() {
                    // There is no chance of this other type conflicting with anything, we can
                    // go to the next one
                    continue 'other_ident;
                }
                if bad_edges.contains(edge_b) {
                    // If we've already processed this edge, go to the next one
                    continue 'next_edge;
                }
                if let Some(edge_c) = edges_with_other_ident.iter().find(|edge_c| {
                    edge_b
                        .instantiation
                        .iter()
                        .zip(edge_c.instantiation.iter())
                        .any(|(arg_b, arg_c)| {
                            match (edge_b.ctx.has_arg(arg_b), edge_c.ctx.has_arg(arg_c)) {
                                // If they are both generic arguments, then they conflict if they're
                                // different arguments
                                (true, true) => arg_b != arg_c,
                                // If only one is generic, then it's a conflict
                                (true, false) | (false, true) => true,
                                // If both are not generic, there is no conflict
                                (false, false) => false,
                            }
                        })
                }) {
                    for bad_edge in [edge_b.clone(), edge_c.clone()] {
                        if !bad_edge.ctx.is_empty() {
                            // We should only filter the edges that have a generic context; this
                            // way, we don't filter the edges that won't conflict
                            let is_new = bad_edges.insert(bad_edge);
                            if is_new {
                                eprintln!(
                                    "// Node {ident}: filtering {other_ident} due to multiple conflicting generic contexts",
                                );
                            }
                        }
                    }
                }
            }
        }

        let node = nodes.0.get_mut(ident).unwrap();
        for field in node.fields_mut() {
            field.tys.retain(|ty| !bad_edges.contains(ty));
            field.indirect_tys.retain(|ty| !bad_edges.contains(ty));
        }

        bad_edges.clear();

        // TODO: in writing my blog post I realized the correct place to do the topological sort is here, not above.

        // 2. Remove all outgoing edges where there's other edges w/ overlapping generic contexts
        //    that cannot be transformed.
        let node = nodes.0.get(ident).unwrap();
        let tys = node.unrestricted_tys().collect::<HashSet<_>>();
        let direct_tys = node.direct_tys().collect::<HashSet<_>>();
        // Then, we'll look for pairs of edges where the contexts intersect, and the latter edge is
        // a direct edge.
        for edge_b in tys.iter() {
            for edge_c in direct_tys
                .iter()
                .filter(|edge_c| edge_b != *edge_c && edge_b.ctx.intersects(&edge_c.ctx))
            {
                let node_c = &nodes[edge_c.ident];
                // Look for an edge c -> b. If we can't find it, then a -> b is a bad edge.
                if !node_c
                    .unrestricted_tys()
                    .map(|ty| collapse_ty_edge(node.ctx(), edge_c, node_c, ty))
                    .any(|ty| &&ty == edge_b)
                {
                    let edge_b_ident = edge_b.ident;
                    let edge_c_ident = edge_c.ident;
                    eprintln!(
                        "// Node {ident}: filtering {edge_b_ident} due to {edge_c_ident} not being able to be transformed by it"
                    );
                    bad_edges.insert((*edge_b).clone());
                }
            }
        }

        let node = nodes.0.get_mut(ident).unwrap();
        for field in node.fields_mut() {
            let mut restrict_ty = |ty: &AType<'ast>| {
                let bad = bad_edges.contains(ty);
                if bad {
                    field.restricted_tys.insert(ty.clone());
                }
                !bad
            };
            field.tys.retain(&mut restrict_ty);
            field.indirect_tys.retain(&mut restrict_ty);
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
                    node.unrestricted_tys()
                        .collect::<HashSet<_>>()
                        .into_iter()
                        .map(|ty| labels.iter().position(|label| label == ty.ident).unwrap())
                        .collect::<Vec<_>>()
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
