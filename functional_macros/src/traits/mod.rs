//! These are all the available traits that can be automatically derived. Each of these modules
//! contains code that will, given the graph of nodes, output all the trait implementations as
//! items.

#[cfg(any(feature = "foldable", feature = "foldable-mut"))]
pub mod foldable;

#[cfg(feature = "foldable-mut")]
pub mod foldable_mut;

#[cfg(feature = "functor")]
pub mod functor;

#[cfg(feature = "try-functor")]
pub mod try_functor;

#[cfg(any(feature = "visit", feature = "visit-mut"))]
pub mod visit;

#[cfg(feature = "visit-mut")]
pub mod visit_mut;
