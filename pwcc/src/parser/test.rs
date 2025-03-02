//! TODO: eventually, I want to have this test the span generation too. However, that's a bit
//! tricky, so I am leaving everything in place for now.

use super::*;
use std::fmt::Debug;

fn lex(source: &str) -> Vec<Token> {
    crate::lexer::lex(source)
        .expect("lex failed")
        .into_iter()
        .map(|s| s.inner)
        .collect()
}

fn assert_forwards<T: FromTokens + Debug + PartialEq>(tokens: &[Token], expected: &T) {
    let actual = T::from_raw_tokens(&mut Vec::from(tokens).into_iter());
    assert_eq!(Ok(expected), actual.as_ref());
}

fn assert_backwards(tree: impl ToTokens + Debug + PartialEq, expected: &[Token]) {
    let actual = tree.to_tokens().collect::<Vec<_>>();
    assert_eq!(expected, actual);
}

fn assert_convertible(s: &str, tree: impl ToTokens + FromTokens + Debug + PartialEq) {
    let tokens = lex(s);
    assert_forwards(&tokens, &tree);
    assert_backwards(tree, &tokens);
}

fn assert_to_from(to: &str, tree: impl ToTokens + FromTokens + Debug + PartialEq, from: &str) {
    let to_tokens = lex(to);
    assert_forwards(&to_tokens, &tree);
    let from_tokens = lex(from);
    assert_backwards(tree, &from_tokens);
}

#[test]
fn constant() {
    assert_convertible("2", Exp::Constant { constant: 2 });
}

#[test]
fn statement() {
    assert_convertible(
        "return 2;",
        Statement::ReturnStmt(ReturnStmt {
            exp: Exp::Constant { constant: 2 }.span((0, 0).into()),
        }),
    );
}

#[test]
fn function() {
    assert_convertible(
        "\
int main(void) {
    int x = 1;
    return 2;
}",
        Function {
            name: "main".to_string().span((0, 0).into()),
            body: Block {
                items: vec![
                    BlockItem::Declaration(Declaration {
                        name: "x".to_string().span((0, 0).into()),
                        init: Initializer::ExpressionInit(ExpressionInit {
                            exp: Exp::Constant { constant: 1 }.span((0, 0).into()),
                        })
                        .span((0, 0).into()),
                    }),
                    BlockItem::Statement(Statement::ReturnStmt(ReturnStmt {
                        exp: Exp::Constant { constant: 2 }.span((0, 0).into()),
                    })),
                ]
                .span((0, 0).into()),
            }
            .span((0, 0).into()),
        },
    );
}

#[test]
fn unary() {
    assert_to_from(
        "- -~3",
        Exp::Unary {
            op: UnaryOp::PrefixOp(PrefixOp::Minus).span((0, 0).into()),
            exp: Exp::Unary {
                op: UnaryOp::PrefixOp(PrefixOp::Minus).span((0, 0).into()),
                exp: Exp::Unary {
                    op: UnaryOp::PrefixOp(PrefixOp::Tilde).span((0, 0).into()),
                    exp: Exp::Constant { constant: 3 }.boxed().span((0, 0).into()),
                }
                .boxed()
                .span((0, 0).into()),
            }
            .boxed()
            .span((0, 0).into()),
        },
        "-(-(~(3)))",
    )
}

#[test]
fn binary() {
    assert_convertible(
        "((1)+(2))*(3)",
        Exp::Binary {
            lhs: Exp::Binary {
                lhs: Exp::Constant { constant: 1 }.boxed().span((0, 0).into()),
                op: BinaryOp::Plus.span((0, 0).into()),
                rhs: Exp::Constant { constant: 2 }.boxed().span((0, 0).into()),
            }
            .boxed()
            .span((0, 0).into()),
            op: BinaryOp::Star.span((0, 0).into()),
            rhs: Exp::Constant { constant: 3 }.boxed().span((0, 0).into()),
        },
    );
}

#[test]
fn binary_precedence() {
    let tokens = lex("1*2-3*(4+5)");
    assert_forwards(
        &tokens,
        &Exp::Binary {
            lhs: Exp::Binary {
                lhs: Exp::Constant { constant: 1 }.boxed().span((0, 0).into()),
                op: BinaryOp::Star.span((0, 0).into()),
                rhs: Exp::Constant { constant: 2 }.boxed().span((0, 0).into()),
            }
            .boxed()
            .span((0, 0).into()),
            op: BinaryOp::Minus.span((0, 0).into()),
            rhs: Exp::Binary {
                lhs: Exp::Constant { constant: 3 }.boxed().span((0, 0).into()),
                op: BinaryOp::Star.span((0, 0).into()),
                rhs: Exp::Binary {
                    lhs: Exp::Constant { constant: 4 }.boxed().span((0, 0).into()),
                    op: BinaryOp::Plus.span((0, 0).into()),
                    rhs: Exp::Constant { constant: 5 }.boxed().span((0, 0).into()),
                }
                .boxed()
                .span((0, 0).into()),
            }
            .boxed()
            .span((0, 0).into()),
        },
    )
}

#[test]
fn binary_associativity() {
    assert_to_from(
        "3/2/1",
        Exp::Binary {
            lhs: Exp::Binary {
                lhs: Exp::Constant { constant: 3 }.boxed().span((0, 0).into()),
                op: BinaryOp::ForwardSlash.span((0, 0).into()),
                rhs: Exp::Constant { constant: 2 }.boxed().span((0, 0).into()),
            }
            .boxed()
            .span((0, 0).into()),
            op: BinaryOp::ForwardSlash.span((0, 0).into()),
            rhs: Exp::Constant { constant: 1 }.boxed().span((0, 0).into()),
        },
        "((3)/(2))/(1)",
    );
}

#[test]
fn assign_precedence() {
    assert_convertible(
        "a = b = c",
        Exp::Assignment {
            lhs: Exp::Var { ident: "a".into() }.boxed().span((0, 0).into()),
            op: AssignmentOp::Equal.span((0, 0).into()),
            rhs: Exp::Assignment {
                lhs: Exp::Var { ident: "b".into() }.boxed().span((0, 0).into()),
                op: AssignmentOp::Equal.span((0, 0).into()),
                rhs: Exp::Var { ident: "c".into() }.boxed().span((0, 0).into()),
            }
            .boxed()
            .span((0, 0).into()),
        },
    )
}

#[test]
fn decl_no_init() {
    assert_convertible(
        "int x;",
        Declaration {
            name: "x".to_string().span((0, 0).into()),
            init: Initializer::NoInit(NoInit {}).span((0, 0).into()),
        },
    );
}

#[test]
fn decl_init() {
    assert_convertible(
        "int x = 5;",
        Declaration {
            name: "x".to_string().span((0, 0).into()),
            init: Initializer::ExpressionInit(ExpressionInit {
                exp: Exp::Constant { constant: 5 }.span((0, 0).into()),
            })
            .span((0, 0).into()),
        },
    );
}

#[test]
fn assign_statement() {
    assert_convertible(
        "sex = 69;",
        Statement::ExpressionStmt(ExpressionStmt {
            exp: Exp::Assignment {
                lhs: Exp::Var {
                    ident: "sex".to_string(),
                }
                .boxed()
                .span((0, 0).into()),
                op: AssignmentOp::Equal.span((0, 0).into()),
                rhs: Exp::Constant { constant: 69 }.boxed().span((0, 0).into()),
            }
            .span((0, 0).into()),
        }),
    );
}

#[test]
fn block_item() {
    assert_convertible(
        "int x = 5;",
        BlockItem::Declaration(Declaration {
            name: "x".to_string().span((0, 0).into()),
            init: Initializer::ExpressionInit(ExpressionInit {
                exp: Exp::Constant { constant: 5 }.span((0, 0).into()),
            })
            .span((0, 0).into()),
        }),
    );

    assert_convertible(
        "sex = 69;",
        BlockItem::Statement(Statement::ExpressionStmt(ExpressionStmt {
            exp: Exp::Assignment {
                lhs: Exp::Var {
                    ident: "sex".into(),
                }
                .boxed()
                .span((0, 0).into()),
                op: AssignmentOp::Equal.span((0, 0).into()),
                rhs: Exp::Constant { constant: 69 }.boxed().span((0, 0).into()),
            }
            .span((0, 0).into()),
        })),
    );
}

#[test]
fn postfix_precedence() {
    assert_to_from(
        "x--++",
        Exp::Unary {
            op: UnaryOp::PostfixOp(PostfixOp::Increment).span((0, 0).into()),
            exp: Exp::Unary {
                op: UnaryOp::PostfixOp(PostfixOp::Decrement).span((0, 0).into()),
                exp: Exp::Var { ident: "x".into() }.boxed().span((0, 0).into()),
            }
            .boxed()
            .span((0, 0).into()),
        },
        "((x)--)++",
    );
}

#[test]
fn prefix_precedence() {
    assert_to_from(
        "--++x",
        Exp::Unary {
            op: UnaryOp::PrefixOp(PrefixOp::Decrement).span((0, 0).into()),
            exp: Exp::Unary {
                op: UnaryOp::PrefixOp(PrefixOp::Increment).span((0, 0).into()),
                exp: Exp::Var { ident: "x".into() }.boxed().span((0, 0).into()),
            }
            .boxed()
            .span((0, 0).into()),
        },
        "--(++(x))",
    );
}

#[test]
fn prefix_postfix_precedence() {
    assert_to_from(
        "--++x--++",
        Exp::Unary {
            op: UnaryOp::PrefixOp(PrefixOp::Decrement).span((0, 0).into()),
            exp: Exp::Unary {
                op: UnaryOp::PrefixOp(PrefixOp::Increment).span((0, 0).into()),
                exp: Exp::Unary {
                    op: UnaryOp::PostfixOp(PostfixOp::Increment).span((0, 0).into()),
                    exp: Exp::Unary {
                        op: UnaryOp::PostfixOp(PostfixOp::Decrement).span((0, 0).into()),
                        exp: Exp::Var { ident: "x".into() }.boxed().span((0, 0).into()),
                    }
                    .boxed()
                    .span((0, 0).into()),
                }
                .boxed()
                .span((0, 0).into()),
            }
            .boxed()
            .span((0, 0).into()),
        },
        "--(++(((x)--)++))",
    );
}

#[test]
fn if_statement() {
    let tokens = lex("if (a) if (a > 10) return a; else return 10 - a;");
    assert_forwards(
        &tokens,
        &Statement::IfStmt(IfStmt {
            guard: Exp::Var {
                ident: "a".to_string(),
            }
            .span((0, 0).into()),
            body: Statement::IfStmt(IfStmt {
                guard: Exp::Binary {
                    lhs: Exp::Var {
                        ident: "a".to_string(),
                    }
                    .boxed()
                    .span((0, 0).into()),
                    op: BinaryOp::GreaterThan.span((0, 0).into()),
                    rhs: Exp::Constant { constant: 10 }.boxed().span((0, 0).into()),
                }
                .span((0, 0).into()),
                body: Statement::ReturnStmt(ReturnStmt {
                    exp: Exp::Var {
                        ident: "a".to_string(),
                    }
                    .span((0, 0).into()),
                })
                .span((0, 0).into())
                .boxed(),
                else_stmt: Some(ElseStmt {
                    body: Statement::ReturnStmt(ReturnStmt {
                        exp: Exp::Binary {
                            lhs: Exp::Constant { constant: 10 }.boxed().span((0, 0).into()),
                            op: BinaryOp::Minus.span((0, 0).into()),
                            rhs: Exp::Var {
                                ident: "a".to_string(),
                            }
                            .boxed()
                            .span((0, 0).into()),
                        }
                        .span((0, 0).into()),
                    })
                    .span((0, 0).into())
                    .boxed(),
                })
                .span((0, 0).into()),
            })
            .span((0, 0).into())
            .boxed(),
            else_stmt: None.span(SourceSpan::empty()),
        }),
    );
}

#[test]
fn if_statement_naked() {
    let tokens = lex("int main(void) { if (0) return a; }");
    assert_forwards(
        &tokens,
        &Function {
            name: "main".to_string().span((0, 0).into()),
            body: Block {
                items: vec![BlockItem::Statement(Statement::IfStmt(IfStmt {
                    guard: Exp::Constant { constant: 0 }.span((0, 0).into()),
                    body: Statement::ReturnStmt(ReturnStmt {
                        exp: Exp::Var {
                            ident: "a".to_string(),
                        }
                        .span((0, 0).into()),
                    })
                    .span((0, 0).into())
                    .boxed(),
                    else_stmt: None.span(SourceSpan::empty()),
                }))]
                .span((0, 0).into()),
            }
            .span((0, 0).into()),
        },
    );
}
