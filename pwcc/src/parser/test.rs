use super::*;

fn assert_forwards<T: FromTokens + Debug + PartialEq>(tokens: &[Token], expected: &T) {
    let actual = T::from_tokens(&mut Vec::from(tokens).into_iter());
    assert_eq!(Ok(expected), actual.as_ref());
}

fn assert_backwards(tree: impl ToTokens + Debug + PartialEq, expected: &[Token]) {
    let actual = tree.to_tokens().collect::<Vec<_>>();
    assert_eq!(expected, actual);
}

fn assert_convertible(s: &str, tree: impl ToTokens + FromTokens + Debug + PartialEq) {
    let tokens = crate::lexer::lex(s).expect("lex failed");
    assert_forwards(&tokens, &tree);
    assert_backwards(tree, &tokens);
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
    assert_convertible(
        "-(-(~(3)))",
        Exp::Unary {
            op: UnaryOp::Minus,
            exp: Box::new(Exp::Unary {
                op: UnaryOp::Minus,
                exp: Box::new(Exp::Unary {
                    op: UnaryOp::Tilde,
                    exp: Box::new(Exp::Constant { constant: 3 }),
                }),
            }),
        },
    )
}

#[test]
fn binary() {
    assert_convertible(
        "((1)+(2))*(3)",
        Exp::Binary {
            lhs: Box::new(Exp::Binary {
                lhs: Box::new(Exp::Constant { constant: 1 }),
                op: BinaryOp::Plus,
                rhs: Box::new(Exp::Constant { constant: 2 }),
            }),
            op: BinaryOp::Star,
            rhs: Box::new(Exp::Constant { constant: 3 }),
        },
    );
}

#[test]
fn binary_precedence() {
    let tokens = crate::lexer::lex("1*2-3*(4+5)").expect("lex failed");
    assert_forwards(
        &tokens,
        &Exp::Binary {
            lhs: Box::new(Exp::Binary {
                lhs: Box::new(Exp::Constant { constant: 1 }),
                op: BinaryOp::Star,
                rhs: Box::new(Exp::Constant { constant: 2 }),
            }),
            op: BinaryOp::Minus,
            rhs: Box::new(Exp::Binary {
                lhs: Box::new(Exp::Constant { constant: 3 }),
                op: BinaryOp::Star,
                rhs: Box::new(Exp::Binary {
                    lhs: Box::new(Exp::Constant { constant: 4 }),
                    op: BinaryOp::Plus,
                    rhs: Box::new(Exp::Constant { constant: 5 }),
                }),
            }),
        },
    )
}

#[test]
fn assign_precedence() {
    assert_convertible(
        "a = b = c",
        Exp::Assignment {
            lhs: Box::new(Exp::Var { ident: "a".into() }),
            rhs: Box::new(Exp::Assignment {
                lhs: Box::new(Exp::Var { ident: "b".into() }),
                rhs: Box::new(Exp::Var { ident: "c".into() }),
            }),
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
                lhs: Box::new(Exp::Var {
                    ident: "sex".into(),
                }),
                rhs: Box::new(Exp::Constant { constant: 69 }),
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
                lhs: Box::new(Exp::Var {
                    ident: "sex".into(),
                }),
                rhs: Box::new(Exp::Constant { constant: 69 }),
            },
        })),
    );
}
