use crate as functional;
use functional::Semigroup;
use functional_macros::ast;

#[ast(typeclasses = [Foldable])]
mod linked_list {
    use super::*;

    #[include()]
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
            &mut |acc: Vec<i32>, node| acc.sconcat(vec![node.val]),
            vec![],
            RecursiveCall::Begin,
        );
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_end() {
        let expected: Vec<i32> = vec![3, 2, 1];
        let actual = example().foldl_impl(
            &mut |acc: Vec<i32>, node| acc.sconcat(vec![node.val]),
            vec![],
            RecursiveCall::End,
        );
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_none() {
        let expected: Vec<i32> = vec![1];
        let actual = example().foldl_impl(
            &mut |acc: Vec<i32>, node| acc.sconcat(vec![node.val]),
            vec![],
            RecursiveCall::None,
        );
        assert_eq!(expected, actual);
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
}
