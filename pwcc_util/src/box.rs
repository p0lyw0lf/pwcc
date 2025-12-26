/// This is a helper trait for constructing Box<T> without having to do Box::new(), which would
/// otherwise create another indentation
pub trait Boxable {
    fn boxed(self) -> Box<Self>;
}

impl<T> Boxable for T {
    #[inline(always)]
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}
