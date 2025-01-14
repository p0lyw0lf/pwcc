//! scc stands for Strongly Connected Component, which this module finds inside a given ANodes
//! directed graph.
//!
//! We want to find strongly connected components in order to create a directed multigraph
//! _between_ the components, traversing it in reverse topological order, in order to apply other passes that trim the graph.
//!
//! An example of why we might want to do so is as follows:
//!
//! ```rust,ignore
//! struct A<T> {
//!     b: B<T>,
//!     c: C<T>,
//! }
//! struct B {
//!     c: C<T>,
//! }
//! struct C<T>(T);
//! ```
//!
//! Then, if you try to write `impl<TInput, TOutput> Functor<B<TOutput>> for A<TInput>`, you have
//! no way of transforming the field `A.c`.
//!
//! However, the other implementation `impl<TInput, TOutput> Functor<C<TOutput>> for A<TInput>`
//! _can_ be written, because the field `A.b` _can_ be transformed if we are able to transform `C`.
//!
//! The question is, how do we distinguish between these cases? My answer is as follows:
//!
//! > An outgoing edge `ab` should be removed if there exists another outgoing edge `ac` such that
//! > 1. The generic context of `ac` has a non-empty intersection with that of `ab`
//! > 2. There exists no edge `cb`.
//!
//! From the example before, it's easy to see how requirement (2) is formed. But what about (1)?
//! Consider the following example:
//!
//! ```rust,ignore
//! struct APrime<T, U> {
//!     b: B<T>,
//!     c: C<U>,
//! }
//! ```
//!
//! Now, the edges `ab` and `ac` do not have overlapping contexts. This means `A.c` doesn't have to
//! be transformed when we are transforming `A.b`. So, requirement (1) is making sure we only look
//! at other edges that need to be transformed (due to the differing input/ouput types).
//!
//! However! There is still a subtlety with requirement (2). Before we start filtering edges, the
//! edge `cb` may exist, and we may end up choosing to keep an edge `ab`. But after we finish
//! filtering, we may have decided to remove `cb`, and should have removed `ab`! So, in order to
//! not keep any edges we should have removed, we need to traverse the nodes in the correct order.
//!
//! I claim the correct traversal order is "reverse topological sort order". Basically, we should
//! start with nodes that don't point to anything, then to nodes that point to nothing but the
//! nodes previously explored, and so on. This ensures we don't end up in the previously explored
//! scenario.
//!
//! However! There's yet another subtlety that will be very familiar to anyone who's worked with
//! graphs: the topological sort order is only defined for directed graphs with no loops. Our graph
//! can have loops, and indeed, that's a big feature for this crate in general.
//!
//! Which finally brings us to why we want to find the strongly connected components! By finding
//! strongly connected components, we can do a topological sort on _those_, which will not have
//! loops, and our traversal order will be good.
//!
//! "But what about within an SCC?", you may ask. Well, thanks to our lattice step earlier, every
//! loop in the graph will become a clique, so if `a`, `b`, and `c` are in the same strongly
//! connected component, then `cb` will always exist.
//!
//! And with that, I believe my informal proof is complete! Now all that's left to do is to do it.

use std::collections::hash_map;
use std::collections::HashMap;
use std::collections::HashSet;

use syn::Ident;

use crate::nodes::lattice::Lattice;
use crate::nodes::ANode;

/// We're using the [Path-based strong component algorithm](https://en.wikipedia.org/wiki/Path-based_strong_component_algorithm) as outlined on Wikipedia. This datastructure will be the output of the algorithm: a list of all connected components.
///
/// We do not care about precise links between connected components, because we will just recover
/// them by traversing nodes anyways.
pub(crate) struct StronglyConnectedComponents<'ast> {
    /// The underlying graph.
    pub nodes: Lattice<'ast>,
    /// Maps the identifier of a node to the index of its connected component.
    /// INVARIANT: \forall i. i \in nodes <=> i \in node_to_index
    pub node_to_index: HashMap<&'ast Ident, usize>,
    /// List of all the strongly connected components.
    /// INVARIANT: \forall i. i \in node_to_index => i \in index_to_scc[node_to_index[i]]
    pub index_to_scc: Vec<HashSet<&'ast Ident>>,
}

pub(crate) fn find_sccs<'ast>(nodes: Lattice<'ast>) -> StronglyConnectedComponents<'ast> {
    struct State<'ast> {
        // Just like the fields in StronglyConnectedComponents, but we can't own `nodes` in this
        // struct, so we have to break them out.
        node_to_index: HashMap<&'ast Ident, usize>,
        index_to_scc: Vec<HashSet<&'ast Ident>>,

        node_to_preorder_num: HashMap<&'ast Ident, usize>,
        preorder_num: usize,

        // All vertices not assigned a strongly connected component. S in the wikipedia article.
        frontier: Vec<&'ast Ident>,
        // All vertices not yet determined to belong to different sccs from each other. P in the
        // wikipedia article.
        path: Vec<&'ast Ident>,
    }

    impl<'ast> State<'ast> {
        /// A depth-first search that implements the algorithm. Returns `None` if the node was
        /// newly discovered, else returns `Some(preorder_number)`.
        fn search(&mut self, nodes: &Lattice<'ast>, node: &ANode<'ast>) -> Option<usize> {
            match self.node_to_preorder_num.entry(node.ident()) {
                hash_map::Entry::Occupied(entry) => {
                    // Node already present, no need to explore
                    return Some(*entry.get());
                }
                hash_map::Entry::Vacant(entry) => {
                    // 1. Set the preorder number of v to C, and increment C.
                    entry.insert(self.preorder_num);
                    self.preorder_num += 1;
                }
            };

            // 2. Push v onto S and also onto P.
            self.frontier.push(node.ident());
            self.path.push(node.ident());

            // 3. For each edge from v to a neighboring vertex w:
            for ty in node.all_tys() {
                let next_node = nodes.get(ty.ident).unwrap();
                match self.search(nodes, next_node) {
                    // If the preorder number of w has not yet been assigned, recursively search w
                    None => {}
                    // Otherwise, if w has not yet been assigned a strongly connected component,
                    Some(existing_preorder)
                        if self.node_to_index.get(next_node.ident()).is_none() =>
                    {
                        // Repeatedly pop vertices from P until the top element of P has a preorder
                        // number less than or equal to the preorder number of w.
                        while let Some(ident) = self.path.last() {
                            let last_node_preorder = *self.node_to_preorder_num.get(ident).unwrap();
                            if last_node_preorder <= existing_preorder {
                                break;
                            }
                            let _ = self.path.pop();
                        }
                    }
                    _ => {}
                }
            }

            // 4. If v is the top element of P:
            if self.path.last() == Some(&node.ident()) {
                // Pop vertices from S until v as been popped, and assign the popped vertices to a
                // new connected component.
                let mut scc = HashSet::new();
                let index = self.index_to_scc.len();

                while let Some(ident) = self.frontier.pop() {
                    scc.insert(ident);
                    self.node_to_index.insert(ident, index);

                    if ident == node.ident() {
                        break;
                    }
                }

                self.index_to_scc.push(scc);

                // Pop v from P
                let _ = self.path.pop();
            }

            None
        }
    }

    let mut state = State {
        node_to_index: Default::default(),
        index_to_scc: Default::default(),

        node_to_preorder_num: Default::default(),
        preorder_num: 0,

        frontier: Default::default(),
        path: Default::default(),
    };

    for node in nodes.values() {
        state.search(&nodes, node);
    }

    StronglyConnectedComponents {
        nodes,
        node_to_index: state.node_to_index,
        index_to_scc: state.index_to_scc,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::nodes::lattice::make_lattice;
    use crate::nodes::test::run_test;

    /// edges[i] contains a list of all other indicies of vertices for outgoing edges
    /// expected_sccs[i] contains the index of the Strongly Connected Component this vertex belongs to.
    fn scc_test(edges: &[&[usize]], expected_sccs: &[usize]) {
        assert_eq!(edges.len(), expected_sccs.len());
        run_test(edges, |nodes, labels| {
            let nodes = make_lattice(nodes);
            let sccs = find_sccs(nodes);

            let actual_sccs = (0..expected_sccs.len())
                .map(|i| sccs.node_to_index[&labels[i]])
                .collect::<Vec<_>>();

            // We need to compare pairwise equality, because the actual indices might be shuffled
            for i in 0..expected_sccs.len() {
                for j in i..expected_sccs.len() {
                    assert_eq!(
                        expected_sccs[i] == expected_sccs[j],
                        actual_sccs[i] == actual_sccs[j]
                    );
                }
            }
        });
    }

    #[test]
    fn simple() {
        scc_test(&[&[1, 2], &[2], &[]], &[0, 1, 2]);
    }

    #[test]
    fn two_disconneted_parts() {
        scc_test(&[&[1], &[0], &[3], &[2]], &[0, 0, 1, 1]);
    }

    #[test]
    fn two_connected_parts() {
        scc_test(&[&[1], &[0, 2], &[3], &[2]], &[0, 0, 1, 1]);
    }
}
