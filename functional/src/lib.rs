pub mod foldable;
pub mod functor;
pub mod try_functor;
pub mod semigroup;

#[cfg(test)]
mod test;

pub use foldable::Foldable;
pub use functor::Functor;
pub use try_functor::TryFunctor;
pub use semigroup::Semigroup;

pub use functional_macros::*;
