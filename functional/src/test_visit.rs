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
    struct C(i32);

    #[test]
    fn test() {
        let x = A {
            b: B { c: C(6) },
            c: C(9),
        };

        impl<'ast> visit::Visit<'ast> for Collect {
            fn visit_c(&mut self, c: &'ast C) {
                self.0.push(c.0);
                visit::visit_c(self, c);
            }
        }

        let mut collect = Collect(vec![]);
        visit::visit_a(&mut collect, &x);
        assert_eq!(collect.0, &[6, 9]);
    }
}

#[ast(typeclasses = [Visit])]
mod recursive {
    use super::*;

    #[include()]
    struct Tree {
        lhs: Option<Box<Tree>>,
        val: i32,
        rhs: Option<Box<Tree>>,
    }

    #[test]
    fn test() {
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

        impl<'ast> visit::Visit<'ast> for Collect {
            fn visit_tree(&mut self, node: &'ast Tree) {
                self.0.push(node.val);
                visit::visit_tree(self, node);
            }
        }

        let mut is = Collect(vec![]);
        visit::visit_tree(&mut is, &x);
        assert_eq!(is.0, &[2, 1, 3]);
    }
}
