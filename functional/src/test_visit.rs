use functional_macros::ast;

#[ast(typeclasses = [Visit])]
mod concrete {
    struct A {
        b: B,
        c: C,
    }
    struct B {
        c: C,
    }
    struct C(i32);

    #[test]
    fn test() {
        let x = A {
            b: B { c: C(6) },
            c: C(9),
        };

        struct Collect(Vec<i32>);
        impl<'ast> visit::Visit<'ast> for Collect {
            fn visit_c(&mut self, c: &'ast C) {
                self.0.push(c.0);
                visit::visit_c(self, c);
            }
        }
    }
}
