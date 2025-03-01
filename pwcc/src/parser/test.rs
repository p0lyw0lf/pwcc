use super::*;

fn lex(source: &str) -> Vec<Token> {
    crate::lexer::lex(source)
        .expect("lex failed")
        .into_iter()
        .map(Into::into)
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
            exp: Exp::Constant { constant: 2 },
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
            name: "main".into(),
            body: Body(Vec::from([
                BlockItem::Declaration(Declaration {
                    name: "x".into(),
                    init: Initializer::ExpressionInit(ExpressionInit {
                        exp: Exp::Constant { constant: 1 },
                    }),
                }),
                BlockItem::Statement(Statement::ReturnStmt(ReturnStmt {
                    exp: Exp::Constant { constant: 2 },
                })),
            ])),
        },
    );
}

#[test]
fn unary() {
    assert_to_from(
        "- -~3",
        Exp::Unary {
            op: UnaryOp::PrefixOp(PrefixOp::Minus),
            exp: Exp::Unary {
                op: UnaryOp::PrefixOp(PrefixOp::Minus),
                exp: Exp::Unary {
                    op: UnaryOp::PrefixOp(PrefixOp::Tilde),
                    exp: Exp::Constant { constant: 3 }.boxed(),
                }
                .boxed(),
            }
            .boxed(),
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
                lhs: Exp::Constant { constant: 1 }.boxed(),
                op: BinaryOp::Plus,
                rhs: Exp::Constant { constant: 2 }.boxed(),
            }
            .boxed(),
            op: BinaryOp::Star,
            rhs: Exp::Constant { constant: 3 }.boxed(),
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
                lhs: Exp::Constant { constant: 1 }.boxed(),
                op: BinaryOp::Star,
                rhs: Exp::Constant { constant: 2 }.boxed(),
            }
            .boxed(),
            op: BinaryOp::Minus,
            rhs: Exp::Binary {
                lhs: Exp::Constant { constant: 3 }.boxed(),
                op: BinaryOp::Star,
                rhs: Exp::Binary {
                    lhs: Exp::Constant { constant: 4 }.boxed(),
                    op: BinaryOp::Plus,
                    rhs: Exp::Constant { constant: 5 }.boxed(),
                }
                .boxed(),
            }
            .boxed(),
        },
    )
}

#[test]
fn assign_precedence() {
    assert_convertible(
        "a = b = c",
        Exp::Assignment {
            lhs: Exp::Var { ident: "a".into() }.boxed(),
            op: AssignmentOp::Equal,
            rhs: Exp::Assignment {
                lhs: Exp::Var { ident: "b".into() }.boxed(),
                op: AssignmentOp::Equal,
                rhs: Exp::Var { ident: "c".into() }.boxed(),
            }
            .boxed(),
        },
    )
}

#[test]
fn decl_no_init() {
    assert_convertible(
        "int x;",
        Declaration {
            name: "x".into(),
            init: Initializer::NoInit(NoInit {}),
        },
    );
}

#[test]
fn decl_init() {
    assert_convertible(
        "int x = 5;",
        Declaration {
            name: "x".into(),
            init: Initializer::ExpressionInit(ExpressionInit {
                exp: Exp::Constant { constant: 5 },
            }),
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
                    ident: "sex".into(),
                }
                .boxed(),
                op: AssignmentOp::Equal,
                rhs: Exp::Constant { constant: 69 }.boxed(),
            },
        }),
    );
}

#[test]
fn block_item() {
    assert_convertible(
        "int x = 5;",
        BlockItem::Declaration(Declaration {
            name: "x".into(),
            init: Initializer::ExpressionInit(ExpressionInit {
                exp: Exp::Constant { constant: 5 },
            }),
        }),
    );

    assert_convertible(
        "sex = 69;",
        BlockItem::Statement(Statement::ExpressionStmt(ExpressionStmt {
            exp: Exp::Assignment {
                lhs: Exp::Var {
                    ident: "sex".into(),
                }
                .boxed(),
                op: AssignmentOp::Equal,
                rhs: Exp::Constant { constant: 69 }.boxed(),
            },
        })),
    );
}

#[test]
fn postfix_precedence() {
    assert_to_from(
        "x--++",
        Exp::Unary {
            op: UnaryOp::PostfixOp(PostfixOp::Increment),
            exp: Exp::Unary {
                op: UnaryOp::PostfixOp(PostfixOp::Decrement),
                exp: Exp::Var { ident: "x".into() }.boxed(),
            }
            .boxed(),
        },
        "((x)--)++",
    );
}

#[test]
fn prefix_precedence() {
    assert_to_from(
        "--++x",
        Exp::Unary {
            op: UnaryOp::PrefixOp(PrefixOp::Decrement),
            exp: Exp::Unary {
                op: UnaryOp::PrefixOp(PrefixOp::Increment),
                exp: Exp::Var { ident: "x".into() }.boxed(),
            }
            .boxed(),
        },
        "--(++(x))",
    );
}

#[test]
fn prefix_postfix_precedence() {
    assert_to_from(
        "--++x--++",
        Exp::Unary {
            op: UnaryOp::PrefixOp(PrefixOp::Decrement),
            exp: Exp::Unary {
                op: UnaryOp::PrefixOp(PrefixOp::Increment),
                exp: Exp::Unary {
                    op: UnaryOp::PostfixOp(PostfixOp::Increment),
                    exp: Exp::Unary {
                        op: UnaryOp::PostfixOp(PostfixOp::Decrement),
                        exp: Exp::Var { ident: "x".into() }.boxed(),
                    }
                    .boxed(),
                }
                .boxed(),
            }
            .boxed(),
        },
        "--(++(((x)--)++))",
    );
}
