use crate as functional;
use functional_macros::ast;

struct Collect(Vec<i32>);
struct AddOne;

#[ast(typeclasses = [Visit, VisitMut])]
mod concrete {
    use super::*;
    #[derive(Debug, PartialEq)]
    struct A {
        b: B,
        c: C,
    }
    #[derive(Debug, PartialEq)]
    struct B {
        c: C,
    }
    #[derive(Debug, PartialEq)]
    pub struct C(pub i32);

    fn example() -> A {
        A {
            b: B { c: C(6) },
            c: C(9),
        }
    }

    #[test]
    fn test_visit() {
        use visit::Visit;

        let mut cs = Collect(vec![]);
        cs.visit_a(&example());
        assert_eq!(cs.0, &[6, 9]);
    }

    #[test]
    fn test_visit_mut() {
        use visit_mut::VisitMut;
        let mut x_actual = example();
        let x_expected = A {
            b: B { c: C(7) },
            c: C(10),
        };

        AddOne.visit_mut_a(&mut x_actual);
        assert_eq!(x_expected, x_actual);
    }
}

impl<'ast> concrete::visit::Visit<'ast> for Collect {
    fn visit_c(&mut self, c: &'ast concrete::C) {
        self.0.push(c.0);
        concrete::visit::visit_c(self, c);
    }
}

impl<'ast> concrete::visit_mut::VisitMut for AddOne {
    fn visit_mut_c(&mut self, c: &mut concrete::C) {
        c.0 += 1;
        concrete::visit_mut::visit_mut_c(self, c);
    }
}

#[ast(typeclasses = [Visit, VisitMut])]
mod recursive {
    use super::*;

    #[include()]
    #[derive(Debug, PartialEq)]
    pub struct Tree {
        lhs: Option<Box<Tree>>,
        pub val: i32,
        rhs: Option<Box<Tree>>,
    }

    fn example() -> Tree {
        Tree {
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
        }
    }

    #[test]
    fn test_visit() {
        use visit::Visit;
        let mut is = Collect(vec![]);
        is.visit_tree(&example());
        assert_eq!(is.0, &[2, 1, 3]);
    }

    #[test]
    fn test_visit_mut() {
        use visit_mut::VisitMut;
        let mut x_actual = example();
        let x_expected = Tree {
            lhs: Some(Box::new(Tree {
                lhs: None,
                val: 2,
                rhs: None,
            })),
            val: 3,
            rhs: Some(Box::new(Tree {
                lhs: None,
                val: 4,
                rhs: None,
            })),
        };

        AddOne.visit_mut_tree(&mut x_actual);
        assert_eq!(x_expected, x_actual);
    }
}

impl<'ast> recursive::visit::Visit<'ast> for Collect {
    fn visit_tree(&mut self, node: &'ast recursive::Tree) {
        self.0.push(node.val);
        recursive::visit::visit_tree(self, node);
    }
}

impl recursive::visit_mut::VisitMut for AddOne {
    fn visit_mut_tree(&mut self, node: &mut recursive::Tree) {
        node.val += 1;
        recursive::visit_mut::visit_mut_tree(self, node);
    }
}
