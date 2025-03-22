use crate as functional;
use functional_macros::ast;

struct Collect(Vec<i32>);

#[ast(typeclasses = [Visit])]
mod concrete {
    use super::*;
    struct A {
        b: B,
        c: C,
    }
    struct B {
        c: C,
    }
    pub struct C(pub i32);

    #[test]
    fn test() {
        use visit::Visit;
        let x = A {
            b: B { c: C(6) },
            c: C(9),
        };

        let mut cs = Collect(vec![]);
        cs.visit_a(&x);
        assert_eq!(cs.0, &[6, 9]);
    }
}

impl<'ast> concrete::visit::Visit<'ast> for Collect {
    fn visit_c(&mut self, c: &'ast concrete::C) {
        self.0.push(c.0);
        concrete::visit::visit_c(self, c);
    }
}

#[ast(typeclasses = [Visit])]
mod recursive {
    use super::*;

    #[include()]
    pub struct Tree {
        lhs: Option<Box<Tree>>,
        pub val: i32,
        rhs: Option<Box<Tree>>,
    }

    #[test]
    fn test() {
        use visit::Visit;
        let x = Tree {
            lhs: Some(Box::new(Tree {
                lhs: None,
                val: 1,
                rhs: None,
            })),
            val: 2,
            rhs: Some(Box::new(Tree {
                lhs: None,
                val: 3,
                rhs: None,
            })),
        };

        let mut is = Collect(vec![]);
        is.visit_tree(&x);
        assert_eq!(is.0, &[2, 1, 3]);
    }
}

impl<'ast> recursive::visit::Visit<'ast> for Collect {
    fn visit_tree(&mut self, node: &'ast recursive::Tree) {
        self.0.push(node.val);
        recursive::visit::visit_tree(self, node);
    }
}
