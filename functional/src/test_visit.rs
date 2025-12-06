//! TODO: other tests I'd like to write:
//! + guarnateeing that `.chain()` traverses the AST exactly once
//! + checking that pre and post-order things are actually ordered properly

use std::{cell::RefCell, rc::Rc};

use crate as functional;
use functional_macros::ast;

#[derive(Clone)]
struct Collect(Rc<RefCell<Vec<i32>>>);
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
        use visit::VisitExt;

        let mut cs = Collect::default();
        cs.visit_a(&example());
        let out = Rc::into_inner(cs.0).unwrap().into_inner();
        assert_eq!(&out, &[6, 9]);
    }

    #[test]
    fn test_visit_mut() {
        use visit_mut::VisitMutExt;
        let mut x_actual = example();
        let x_expected = A {
            b: B { c: C(7) },
            c: C(10),
        };

        AddOne.visit_mut_a(&mut x_actual);
        assert_eq!(x_expected, x_actual);
    }

    #[test]
    fn test_visit_chain() {
        use visit::VisitExt;

        let cs1 = Collect::default();
        let cs2 = Collect::default();
        {
            let e = example();
            let mut cs = cs1.clone().chain(cs2.clone());
            cs.visit_a(&e);
        }
        let out1 = Rc::into_inner(cs1.0).unwrap().into_inner();
        let out2 = Rc::into_inner(cs2.0).unwrap().into_inner();
        assert_eq!(&out1, &[6, 9]);
        assert_eq!(&out2, &[6, 9]);
    }

    #[test]
    fn test_visit_mut_chain() {
        use visit_mut::VisitMutExt;
        let mut x_actual = example();
        let x_expected = A {
            b: B { c: C(8) },
            c: C(11),
        };

        let mut v = AddOne.chain(AddOne);
        v.visit_mut_a(&mut x_actual);
        assert_eq!(x_expected, x_actual);
    }
}

impl Default for Collect {
    fn default() -> Self {
        Self(Rc::new(RefCell::new(Vec::new())))
    }
}

impl<'ast> concrete::visit::Visit<'ast> for Collect {
    fn visit_c_pre(&mut self, c: &'ast concrete::C) {
        self.0.borrow_mut().push(c.0);
    }
}

impl concrete::visit_mut::VisitMut for AddOne {
    fn visit_mut_c_pre(&mut self, c: &mut concrete::C) {
        c.0 += 1;
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
        use visit::VisitExt;
        let mut is = Collect::default();
        is.visit_tree(&example());
        let out = Rc::into_inner(is.0).unwrap().into_inner();
        assert_eq!(out, &[2, 1, 3]);
    }

    #[test]
    fn test_visit_mut() {
        use visit_mut::VisitMutExt;
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
    fn visit_tree_pre(&mut self, node: &'ast recursive::Tree) {
        self.0.borrow_mut().push(node.val);
    }
}

impl recursive::visit_mut::VisitMut for AddOne {
    fn visit_mut_tree_pre(&mut self, node: &mut recursive::Tree) {
        node.val += 1;
    }
}
