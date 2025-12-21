//! TODO: eventually, I want to have this test the span generation too. However, that's a bit
//! tricky, so I am leaving everything in place for now.

use super::*;
use std::fmt::Debug;

/// Helper constant to fill out struct definitions
#[allow(non_upper_case_globals)]
const span: Span = Span::empty();

fn lex(source: &str) -> Vec<Token> {
    crate::lexer::lex(source)
        .expect("lex failed")
        .into_iter()
        .map(|s| s.0)
        .collect()
}

/// Helper trait to avoid so many layers of Box::new()
trait Boxed {
    fn boxed(self) -> Box<Self>;
}

impl<T> Boxed for T {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

fn assert_forwards<T: FromTokens<Token, ParseError> + Debug + PartialEq>(
    tokens: &[Token],
    expected: &T,
) {
    let actual = T::from_raw_tokens(&mut Vec::from(tokens).into_iter());
    assert_eq!(Ok(expected), actual.as_ref());
}

fn assert_backwards(tree: impl ToTokens<Token> + Debug + PartialEq, expected: &[Token]) {
    let actual = tree.to_tokens().collect::<Vec<_>>();
    assert_eq!(expected, actual);
}

fn assert_convertible(
    s: &str,
    tree: impl ToTokens<Token> + FromTokens<Token, ParseError> + Debug + PartialEq,
) {
    let tokens = lex(s);
    assert_forwards(&tokens, &tree);
    assert_backwards(tree, &tokens);
}

fn assert_to_from(
    to: &str,
    tree: impl ToTokens<Token> + FromTokens<Token, ParseError> + Debug + PartialEq,
    from: &str,
) {
    let to_tokens = lex(to);
    assert_forwards(&to_tokens, &tree);
    let from_tokens = lex(from);
    assert_backwards(tree, &from_tokens);
}

#[test]
fn constant() {
    assert_convertible("2", Exp::Constant { constant: 2, span });
}

#[test]
fn statement() {
    assert_convertible(
        "return 2;",
        Statement::ReturnStmt(ReturnStmt {
            exp: Exp::Constant { constant: 2, span },
            span,
        }),
    );
}

#[test]
fn extra_junk() {
    let tokens = lex("one two three ;")
        .into_iter()
        .map(|token| (token, Span::empty()))
        .collect::<Vec<_>>();
    let mut tokens = as_cloneable(&tokens);
    let labels = Vec::<RawLabel>::from_tokens(&mut tokens).expect("Parse labels");
    println!("{labels:?}");
    let semicolon = NullStmt::from_tokens(&mut tokens).expect("Parse semicolon");
    println!("{semicolon:?}");
}

#[test]
fn function() {
    assert_convertible(
        "\
int main(void) {
    int x = 1;
    return 2;
}",
        FunctionDecl {
            name: ("main".to_string(), span),
            args: FunctionDeclArgs::Void(span),
            body: FunctionBody::Defined(Block {
                items: vec![
                    BlockItem::Declaration(Declaration::Var(VarDecl {
                        name: ("x".to_string(), span),
                        init: Initializer::Defined(Exp::Constant { constant: 1, span }, span),
                        span,
                    })),
                    BlockItem::Statement(Statement::ReturnStmt(ReturnStmt {
                        exp: Exp::Constant { constant: 2, span },
                        span,
                    })),
                ],
                span,
            }),
            span,
        },
    );
}

#[test]
fn unary() {
    assert_to_from(
        "- -~3",
        Exp::Unary {
            op: UnaryOp::Prefix(PrefixOp::Minus(span)),
            exp: Exp::Unary {
                op: UnaryOp::Prefix(PrefixOp::Minus(span)),
                exp: Exp::Unary {
                    op: UnaryOp::Prefix(PrefixOp::BitNot(span)),
                    exp: Exp::Constant { constant: 3, span }.boxed(),
                    span,
                }
                .boxed(),
                span,
            }
            .boxed(),
            span,
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
                lhs: Exp::Constant { constant: 1, span }.boxed(),
                op: BinaryOp::Plus(span),
                rhs: Exp::Constant { constant: 2, span }.boxed(),
                span,
            }
            .boxed(),
            op: BinaryOp::Times(span),
            rhs: Exp::Constant { constant: 3, span }.boxed(),
            span,
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
                lhs: Exp::Constant { constant: 1, span }.boxed(),
                op: BinaryOp::Times(span),
                rhs: Exp::Constant { constant: 2, span }.boxed(),
                span,
            }
            .boxed(),
            op: BinaryOp::Minus(span),
            rhs: Exp::Binary {
                lhs: Exp::Constant { constant: 3, span }.boxed(),
                op: BinaryOp::Times(span),
                rhs: Exp::Binary {
                    lhs: Exp::Constant { constant: 4, span }.boxed(),
                    op: BinaryOp::Plus(span),
                    rhs: Exp::Constant { constant: 5, span }.boxed(),
                    span,
                }
                .boxed(),
                span,
            }
            .boxed(),
            span,
        },
    )
}

#[test]
fn binary_associativity() {
    assert_to_from(
        "3/2/1",
        Exp::Binary {
            lhs: Exp::Binary {
                lhs: Exp::Constant { constant: 3, span }.boxed(),
                op: BinaryOp::Divide(span),
                rhs: Exp::Constant { constant: 2, span }.boxed(),
                span,
            }
            .boxed(),
            op: BinaryOp::Divide(span),
            rhs: Exp::Constant { constant: 1, span }.boxed(),
            span,
        },
        "((3)/(2))/(1)",
    );
}

#[test]
fn assign_precedence() {
    assert_convertible(
        "a = b = c",
        Exp::Assignment {
            lhs: Exp::Var {
                ident: "a".to_string(),
                span,
            }
            .boxed(),
            op: AssignmentOp::Equal(span),
            rhs: Exp::Assignment {
                lhs: Exp::Var {
                    ident: "b".to_string(),
                    span,
                }
                .boxed(),
                op: AssignmentOp::Equal(span),
                rhs: Exp::Var {
                    ident: "c".to_string(),
                    span,
                }
                .boxed(),
                span,
            }
            .boxed(),
            span,
        },
    )
}

#[test]
fn decl_no_init() {
    assert_convertible(
        "int x;",
        Declaration::Var(VarDecl {
            name: ("x".to_string(), span),
            init: Initializer::Declared(span),
            span,
        }),
    );
}

#[test]
fn decl_init() {
    assert_convertible(
        "int x = 5;",
        Declaration::Var(VarDecl {
            name: ("x".to_string(), span),
            init: Initializer::Defined(Exp::Constant { constant: 5, span }, span),
            span,
        }),
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
                    span,
                }
                .boxed(),
                op: AssignmentOp::Equal(span),
                rhs: Exp::Constant { constant: 69, span }.boxed(),
                span,
            },
            span,
        }),
    );
}

#[test]
fn block_item() {
    assert_convertible(
        "int x = 5;",
        BlockItem::Declaration(Declaration::Var(VarDecl {
            name: ("x".to_string(), span),
            init: Initializer::Defined(Exp::Constant { constant: 5, span }, span),
            span,
        })),
    );

    assert_convertible(
        "sex = 69;",
        BlockItem::Statement(Statement::ExpressionStmt(ExpressionStmt {
            exp: Exp::Assignment {
                lhs: Exp::Var {
                    ident: "sex".to_string(),
                    span,
                }
                .boxed(),
                op: AssignmentOp::Equal(span),
                rhs: Exp::Constant { constant: 69, span }.boxed(),
                span,
            },
            span,
        })),
    );
}

#[test]
fn postfix_precedence() {
    assert_to_from(
        "x--++",
        Exp::Unary {
            op: UnaryOp::Postfix(PostfixOp::Increment(span)),
            exp: Exp::Unary {
                op: UnaryOp::Postfix(PostfixOp::Decrement(span)),
                exp: Exp::Var {
                    ident: "x".to_string(),
                    span,
                }
                .boxed(),
                span,
            }
            .boxed(),
            span,
        },
        "((x)--)++",
    );
}

#[test]
fn prefix_precedence() {
    assert_to_from(
        "--++x",
        Exp::Unary {
            op: UnaryOp::Prefix(PrefixOp::Decrement(span)),
            exp: Exp::Unary {
                op: UnaryOp::Prefix(PrefixOp::Increment(span)),
                exp: Exp::Var {
                    ident: "x".to_string(),
                    span,
                }
                .boxed(),
                span,
            }
            .boxed(),
            span,
        },
        "--(++(x))",
    );
}

#[test]
fn prefix_postfix_precedence() {
    assert_to_from(
        "--++x--++",
        Exp::Unary {
            op: UnaryOp::Prefix(PrefixOp::Decrement(span)),
            exp: Exp::Unary {
                op: UnaryOp::Prefix(PrefixOp::Increment(span)),
                exp: Exp::Unary {
                    op: UnaryOp::Postfix(PostfixOp::Increment(span)),
                    exp: Exp::Unary {
                        op: UnaryOp::Postfix(PostfixOp::Decrement(span)),
                        exp: Exp::Var {
                            ident: "x".to_string(),
                            span,
                        }
                        .boxed(),
                        span,
                    }
                    .boxed(),
                    span,
                }
                .boxed(),
                span,
            }
            .boxed(),
            span,
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
                span,
            },
            body_true: Statement::IfStmt(IfStmt {
                guard: Exp::Binary {
                    lhs: Exp::Var {
                        ident: "a".to_string(),
                        span,
                    }
                    .boxed(),
                    op: BinaryOp::GreaterThan(span),
                    rhs: Exp::Constant { constant: 10, span }.boxed(),
                    span,
                },
                body_true: Statement::ReturnStmt(ReturnStmt {
                    exp: Exp::Var {
                        ident: "a".to_string(),
                        span,
                    },
                    span,
                })
                .boxed(),
                body_false: Some(
                    Statement::ReturnStmt(ReturnStmt {
                        exp: Exp::Binary {
                            lhs: Exp::Constant { constant: 10, span }.boxed(),
                            op: BinaryOp::Minus(span),
                            rhs: Exp::Var {
                                ident: "a".to_string(),
                                span,
                            }
                            .boxed(),
                            span,
                        },
                        span,
                    })
                    .boxed(),
                ),
                span,
            })
            .boxed(),
            body_false: None,
            span,
        }),
    );
}

#[test]
fn if_statement_naked() {
    let tokens = lex("int main(void) { if (0) return a; }");
    assert_forwards(
        &tokens,
        &FunctionDecl {
            name: ("main".to_string(), span),
            args: FunctionDeclArgs::Void(span),
            body: FunctionBody::Defined(Block {
                items: vec![BlockItem::Statement(Statement::IfStmt(IfStmt {
                    guard: Exp::Constant { constant: 0, span },
                    body_true: Statement::ReturnStmt(ReturnStmt {
                        exp: Exp::Var {
                            ident: "a".to_string(),
                            span,
                        },
                        span,
                    })
                    .boxed(),
                    body_false: None,
                    span,
                }))],
                span,
            }),
            span,
        },
    );
}

#[test]
fn function_decl_args() {
    assert_convertible(
        "int incr(int x);",
        FunctionDecl {
            name: ("incr".to_string(), span),
            args: FunctionDeclArgs::DeclArgs(CommaDelimited(vec![DeclArg {
                name: ("x".to_string(), span),
                span,
            }])),
            body: FunctionBody::Declared(span),
            span,
        },
    );

    assert_convertible(
        "int cmp(int a,int b);",
        FunctionDecl {
            name: ("cmp".to_string(), span),
            args: FunctionDeclArgs::DeclArgs(CommaDelimited(vec![
                DeclArg {
                    name: ("a".to_string(), span),
                    span,
                },
                DeclArg {
                    name: ("b".to_string(), span),
                    span,
                },
            ])),
            body: FunctionBody::Declared(span),
            span,
        },
    );
}

#[test]
fn function_call_args() {
    assert_convertible(
        "incr(5)",
        Exp::FunctionCall {
            ident: ("incr".to_string(), span),
            args: CommaDelimited(vec![Exp::Constant { constant: 5, span }]),
            span,
        },
    );

    assert_convertible(
        "cmp(4,10)",
        Exp::FunctionCall {
            ident: ("cmp".to_string(), span),
            args: CommaDelimited(vec![
                Exp::Constant { constant: 4, span },
                Exp::Constant { constant: 10, span },
            ]),
            span,
        },
    );
}

#[test]
fn binary_assignment_op() {
    assert_forwards(
        &lex("a &= 0 || b;"),
        &Statement::ExpressionStmt(ExpressionStmt {
            exp: Exp::Assignment {
                lhs: Exp::Var {
                    ident: "a".to_string(),
                    span,
                }
                .boxed(),
                op: AssignmentOp::BitAndEqual(span),
                rhs: Exp::Binary {
                    lhs: Exp::Constant { constant: 0, span }.boxed(),
                    op: BinaryOp::LogicOr(span),
                    rhs: Exp::Var {
                        ident: "b".to_string(),
                        span,
                    }
                    .boxed(),
                    span,
                }
                .boxed(),
                span,
            },
            span,
        }),
    );
}

#[test]
fn trailing_comma_exp() {
    let tokens = lex("f(a, b, c,)");
    let tokens = tokens
        .into_iter()
        .map(|token| (token, Span::empty()))
        .collect::<Vec<_>>();
    let mut iter = as_cloneable(&tokens);
    let _ = Exp::from_tokens(&mut iter).expect("parse succeeds"); // because it can parse f as ident
    assert_eq!(
        Some(&Token::OpenParen),
        iter.clone().next().map(|(token, _)| token)
    );
}

#[test]
fn trailing_comma_decl() {
    let tokens = lex("int ident(int a,) { return a; }");
    let _ = FunctionDecl::from_raw_tokens(&mut tokens.into_iter()).expect_err("parse did not fail");
    // TODO: also test that error is "good" (currently it is not very helpful unfortunately...)
}

#[test]
fn empty_args_exp() {
    assert_convertible(
        "f()",
        Exp::FunctionCall {
            ident: ("f".to_string(), span),
            args: CommaDelimited(vec![]),
            span,
        },
    );
}

#[test]
fn empty_args_decl() {
    assert_convertible(
        "int f();",
        FunctionDecl {
            name: ("f".to_string(), span),
            args: FunctionDeclArgs::DeclArgs(CommaDelimited(vec![])),
            body: FunctionBody::Declared(span),
            span,
        },
    );
}
