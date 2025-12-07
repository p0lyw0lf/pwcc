/// Required to avoid conflicting trait implementation problems for the standard library's tuple
/// tupe. This lets us do fun trait-level recursion on this type when previously we could not.
pub struct Tuple<A, B> {
    pub fst: A,
    pub snd: B,
}

#[macro_export]
macro_rules! tuple {
    ($a:expr, $b:expr) => {
        $crate::Tuple { fst: $a, snd: $b }
    };
}
