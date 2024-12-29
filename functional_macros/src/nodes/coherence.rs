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

use syn::Ident;

use crate::nodes::scc::StronglyConnectedComponents;
use crate::nodes::ANode;
use crate::nodes::ANodes;

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
                .map(|ident| sccs.nodes[ident].tys())
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

pub fn filter_coherent<'ast>(sccs: StronglyConnectedComponents<'ast>) -> ANodes<'ast> {
    let order = topological_sort(&sccs);
    println!("{order:?}");
    todo!()
}

#[cfg(test)]
mod test {
    use syn::ExprReference;

    use super::*;

    use crate::nodes::lattice::make_lattice;
    use crate::nodes::scc::find_sccs;
    use crate::nodes::test::run_test;

    /// edges[i] contains a list of all other indicies of vertices for outgoing edges
    /// expected_edges[i] is what we expect the edge list to look like after filtering
    fn coherence_test(edges: &[&[usize]], expected_edges: &[&[usize]]) {
        assert_eq!(edges.len(), expected_edges.len());

        run_test(edges, |nodes, labels| {
            let nodes = make_lattice(nodes);
            let sccs = find_sccs(nodes);
            let nodes = filter_coherent(sccs);
        });
    }

    #[test]
    fn simple() {
        // This is the example I reference everywhere
        coherence_test(&[&[1, 2], &[2], &[]], &[&[2], &[2], &[]]);
    }
}
