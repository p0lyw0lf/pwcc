//! scc stands for Strongly Connected Component, which this module finds inside a given ANodes
//! directed graph.
//!
//! We want to find strongly connected components in order to create a directed multigraph
//! _between_ the components, traversing it in reverse topological order, in order to apply other passes that trim the graph.
//!
//! An example of why we might want to do so is as follows:
//!
//! ```rust
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
//! ```rust
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
