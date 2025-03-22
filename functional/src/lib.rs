mod control_flow;
mod foldable;
mod functor;
mod semigroup;
mod try_functor;

#[cfg(test)]
mod test_foldable;
#[cfg(test)]
mod test_functor;
#[cfg(test)]
mod test_visit;

pub use control_flow::ControlFlow;
pub use control_flow::RecursiveCall;
pub use foldable::Foldable;
pub use functor::Functor;
pub use semigroup::Semigroup;
pub use try_functor::TryFunctor;

pub use functional_macros::*;

macro_rules! base {
    ($($ty:ident),*) => {
        functor_base!($($ty,)*);
        try_functor_base!($($ty,)*);
    }
}

// All the main primitive types
base!(bool, char, f32, f64, i8, i16, i32, i64, isize, u8, u16, u32, u64, usize);
// Other prelude types
base!(String);
