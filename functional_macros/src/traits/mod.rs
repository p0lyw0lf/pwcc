//! These are all the available traits that can be automatically derived. Each of these modules
//! contains code that will, given the graph of nodes, output all the trait implementations as
//! items.

#[cfg(feature = "foldable")]
pub mod foldable;

#[cfg(any(feature = "functor", feature = "try_functor"))]
pub mod functor;

#[cfg(feature = "try_functor")]
pub mod try_functor;
