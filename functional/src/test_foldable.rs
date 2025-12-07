use crate as functional;
use functional::Semigroup;
use functional_macros::ast;

#[ast(typeclasses = [Foldable, FoldableMut])]
mod linked_list {
    use super::*;

    #[include()]
    #[derive(Debug, PartialEq)]
    struct Node<T> {
        val: T,
        next: Option<Box<Node<T>>>,
    }

    fn example() -> Node<i32> {
        Node {
            val: 1,
            next: Some(Box::new(Node {
                val: 2,
                next: Some(Box::new(Node { val: 3, next: None })),
            })),
        }
    }

    #[test]
    fn test_begin() {
        let expected: Vec<i32> = vec![1, 2, 3];
        let actual = example().foldl_impl(
            &mut |mut acc: Vec<i32>, node| {
                acc.sconcat(vec![node.val]);
                acc
            },
            vec![],
            RecursiveCall::Begin,
        );
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_end() {
        let expected: Vec<i32> = vec![3, 2, 1];
        let actual = example().foldl_impl(
            &mut |mut acc: Vec<i32>, node| {
                acc.sconcat(vec![node.val]);
                acc
            },
            vec![],
            RecursiveCall::End,
        );
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_none() {
        let expected: Vec<i32> = vec![1];
        let actual = example().foldl_impl(
            &mut |mut acc: Vec<i32>, node| {
                acc.sconcat(vec![node.val]);
                acc
            },
            vec![],
            RecursiveCall::None,
        );
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_mut() {
        let mut x_actual = example();
        x_actual.foldl_mut(
            |(), n| {
                n.val += 1;
            },
            (),
        );
        let x_expected = Node {
            val: 2,
            next: Some(Box::new(Node {
                val: 3,
                next: Some(Box::new(Node { val: 4, next: None })),
            })),
        };
        assert_eq!(x_actual, x_expected);
    }
}

#[ast(typeclasses = [Foldable])]
mod tree {
    use super::*;

    #[include()]
    struct Tree<T> {
        lhs: Option<Box<Tree<T>>>,
        val: T,
        rhs: Option<Box<Tree<T>>>,
    }

    fn example() -> Tree<i32> {
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
    fn test_begin() {
        let expected: Vec<i32> = vec![2, 1, 3];
        let actual = example().foldl_impl(
            &mut |mut acc: Vec<i32>, node| {
                acc.sconcat(vec![node.val]);
                acc
            },
            vec![],
            RecursiveCall::Begin,
        );
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_end() {
        let expected: Vec<i32> = vec![1, 3, 2];
        let actual = example().foldl_impl(
            &mut |mut acc: Vec<i32>, node| {
                acc.sconcat(vec![node.val]);
                acc
            },
            vec![],
            RecursiveCall::End,
        );
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_none() {
        let expected: Vec<i32> = vec![2];
        let actual = example().foldl_impl(
            &mut |mut acc: Vec<i32>, node| {
                acc.sconcat(vec![node.val]);
                acc
            },
            vec![],
            RecursiveCall::None,
        );
        assert_eq!(expected, actual);
    }
}
