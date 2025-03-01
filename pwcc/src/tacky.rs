use core::convert::From;
use std::collections::HashMap;

use crate::parser;

#[derive(Debug)]
pub struct Program {
    pub function: Function,
}

impl From<parser::Program> for Program {
    fn from(program: parser::Program) -> Self {
        Self {
            function: program.function.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Instructions,
}

impl From<parser::Function> for Function {
    fn from(function: parser::Function) -> Self {
        let name = Identifier(function.name);
        Self {
            body: Instructions::from_function(&name, function.body),
            name,
        }
    }
}

#[derive(Debug)]
pub struct Instructions(pub Vec<Instruction>);
#[derive(Debug)]
pub enum Instruction {
    Return {
        val: Val,
    },
    Unary {
        op: UnaryOp,
        src: Val,
        dst: Temporary,
    },
    Binary {
        op: BinaryOp,
        src1: Val,
        src2: Val,
        dst: Temporary,
    },
    Copy {
        src: Val,
        dst: Temporary,
    },
    Jump {
        target: Identifier,
    },
    JumpIfZero {
        condition: Temporary,
        target: Identifier,
    },
    JumpIfNotZero {
        condition: Temporary,
        target: Identifier,
    },
    Label(Identifier),
}

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitAnd,
    BitXor,
    BitOr,
    BitLeftShift,
    BitRightShift,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

#[derive(Debug)]
pub enum Val {
    Constant(isize),
    Var(Temporary),
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Temporary(pub usize);

/// Context needed for emitting instructions
struct ChompContext<'a> {
    instructions: Vec<Instruction>,
    tf: TemporaryFactory<'a>,
}

/// Represents a type that can be used to emit instructions
trait Chompable {
    type Output;
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output;
}

/// Makes a new temporary from the value
impl Chompable for Val {
    type Output = Temporary;
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output {
        match self {
            Self::Var(t) => t,
            src @ Val::Constant(_) => {
                let dst = ctx.tf.next();
                ctx.instructions.push(Instruction::Copy {
                    src,
                    dst: dst.clone(),
                });
                dst
            }
        }
    }
}

impl Instructions {
    fn from_function(identifier: &Identifier, body: parser::Body) -> Self {
        let instructions = Vec::<Instruction>::new();
        let tf = TemporaryFactory::new(identifier);

        let mut ctx = ChompContext { instructions, tf };

        for block_item in body.0 {
            block_item.chomp(&mut ctx);
        }

        let mut instructions = ctx.instructions;

        // See page 113; This will do nothing if the function is well-formed, and guarantees we
        // exit in case there is undefined behavior.
        instructions.push(Instruction::Return {
            val: Val::Constant(0),
        });

        Self(instructions)
    }
}

impl Chompable for parser::BlockItem {
    type Output = ();
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output {
        use parser::BlockItem::*;
        match self {
            Declaration(decl) => decl.chomp(ctx),
            Statement(statement) => statement.chomp(ctx),
        }
    }
}

impl Chompable for parser::Declaration {
    type Output = ();
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output {
        use parser::Initializer::*;
        match self.init {
            NoInit(_) => {}
            ExpressionInit(expression_init) => {
                let src = expression_init.exp.chomp(ctx);
                let dst = ctx.tf.var(&self.name);
                ctx.instructions.push(Instruction::Copy { src, dst });
            }
        }
    }
}

impl Chompable for parser::Statement {
    type Output = ();
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output {
        use parser::Statement::*;
        match self {
            NullStmt(_) => {}
            ReturnStmt(return_statement) => {
                let val = return_statement.exp.chomp(ctx);
                ctx.instructions.push(Instruction::Return { val });
            }
            ExpressionStmt(expression_stmt) => {
                let _ = expression_stmt.exp.chomp(ctx);
            }
            IfStmt(if_stmt) => todo!(),
        }
    }
}

/// Takes in an expression, list of instructions, and the next temporary available, and returns the
/// final value of the expression.
impl Chompable for parser::Exp {
    type Output = Val;
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Val {
        use parser::Exp::*;
        match self {
            Constant { constant } => Val::Constant(constant),
            Unary {
                op:
                    parser::UnaryOp::PrefixOp(
                        op @ (parser::PrefixOp::Minus
                        | parser::PrefixOp::Tilde
                        | parser::PrefixOp::Exclamation),
                    ),
                exp,
            } => {
                let src = exp.chomp(ctx);
                let dst = ctx.tf.next();
                use parser::PrefixOp::*;
                let op = match op {
                    Minus => UnaryOp::Negate,
                    Tilde => UnaryOp::Complement,
                    Exclamation => UnaryOp::Not,
                    _ => unreachable!(),
                };
                ctx.instructions.push(Instruction::Unary { op, src, dst });

                Val::Var(dst)
            }
            Unary {
                op:
                    parser::UnaryOp::PrefixOp(
                        op @ (parser::PrefixOp::Increment | parser::PrefixOp::Decrement),
                    ),
                exp,
            } => match *exp {
                Var { ident } => {
                    let dst = ctx.tf.var(&ident);
                    let src = Val::Var(dst);

                    use parser::PrefixOp::*;
                    ctx.instructions.push(Instruction::Binary {
                        src1: src,
                        op: match op {
                            Increment => BinaryOp::Add,
                            Decrement => BinaryOp::Subtract,
                            _ => unreachable!(),
                        },
                        src2: Val::Constant(1),
                        dst,
                    });
                    Val::Var(dst)
                }
                ref otherwise => {
                    panic!("building tacky: got {op:?} on expression {otherwise:?}")
                }
            },
            Unary {
                op: parser::UnaryOp::PostfixOp(op),
                exp,
            } => match *exp {
                Var { ident } => {
                    let dst = ctx.tf.var(&ident);
                    let src = Val::Var(dst);
                    let tmp = ctx.tf.next();

                    ctx.instructions.push(Instruction::Copy { src, dst: tmp });

                    let src = Val::Var(dst);
                    use parser::PostfixOp::*;
                    ctx.instructions.push(Instruction::Binary {
                        src1: src,
                        op: match op {
                            Increment => BinaryOp::Add,
                            Decrement => BinaryOp::Subtract,
                        },
                        src2: Val::Constant(1),
                        dst,
                    });
                    Val::Var(tmp)
                }
                ref otherwise => {
                    panic!("building tacky: got {op:?} on expression {otherwise:?}")
                }
            },
            Binary {
                lhs,
                op: op @ (parser::BinaryOp::DoubleAmpersand | parser::BinaryOp::DoublePipe),
                rhs,
            } => {
                let is_and = matches!(op, parser::BinaryOp::DoubleAmpersand);

                let shortcut_label = ctx.tf.next_label(if is_and { "false" } else { "true" });

                let src1 = lhs.chomp(ctx).chomp(ctx);
                ctx.instructions.push(if is_and {
                    Instruction::JumpIfZero {
                        condition: src1,
                        target: shortcut_label.clone(),
                    }
                } else {
                    Instruction::JumpIfNotZero {
                        condition: src1,
                        target: shortcut_label.clone(),
                    }
                });

                let src2 = rhs.chomp(ctx).chomp(ctx);
                ctx.instructions.push(if is_and {
                    Instruction::JumpIfZero {
                        condition: src2,
                        target: shortcut_label.clone(),
                    }
                } else {
                    Instruction::JumpIfNotZero {
                        condition: src2,
                        target: shortcut_label.clone(),
                    }
                });

                let dst = ctx.tf.next();
                let end_label = ctx.tf.next_label("end");
                ctx.instructions.extend(
                    [
                        Instruction::Copy {
                            src: Val::Constant(if is_and { 1 } else { 0 }),
                            dst: dst.clone(),
                        },
                        Instruction::Jump {
                            target: end_label.clone(),
                        },
                        Instruction::Label(shortcut_label),
                        Instruction::Copy {
                            src: Val::Constant(if is_and { 0 } else { 1 }),
                            dst: dst.clone(),
                        },
                        Instruction::Label(end_label),
                    ]
                    .into_iter(),
                );

                Val::Var(dst)
            }
            Binary { lhs, op, rhs } => {
                let src1 = lhs.chomp(ctx);
                let src2 = rhs.chomp(ctx);
                let dst = ctx.tf.next();
                use parser::BinaryOp::*;
                let op = match op {
                    Ampersand => BinaryOp::BitAnd,
                    Caret => BinaryOp::BitXor,
                    DoubleAmpersand => unreachable!(),
                    DoubleEqual => BinaryOp::Equal,
                    DoublePipe => unreachable!(),
                    ForwardSlash => BinaryOp::Divide,
                    GreaterThan => BinaryOp::GreaterThan,
                    GreaterThanEqual => BinaryOp::GreaterThanEqual,
                    LeftShift => BinaryOp::BitLeftShift,
                    LessThan => BinaryOp::LessThan,
                    LessThanEqual => BinaryOp::LessThanEqual,
                    Minus => BinaryOp::Subtract,
                    NotEqual => BinaryOp::NotEqual,
                    Percent => BinaryOp::Remainder,
                    Pipe => BinaryOp::BitOr,
                    Plus => BinaryOp::Add,
                    RightShift => BinaryOp::BitRightShift,
                    Star => BinaryOp::Multiply,
                };
                ctx.instructions.push(Instruction::Binary {
                    op,
                    src1,
                    src2,
                    dst,
                });

                Val::Var(dst)
            }
            Var { ident } => Val::Var(ctx.tf.var(&ident)),
            Assignment { lhs, op, rhs } => match *lhs {
                Var { ref ident } => {
                    let dst = ctx.tf.var(ident);
                    let rhs = match op {
                        parser::AssignmentOp::Equal => *rhs,
                        otherwise => parser::Exp::Binary {
                            lhs,
                            op: otherwise
                                .to_binary_op()
                                .expect("to convert AssignmentOp to BinaryOp"),
                            rhs,
                        },
                    };
                    let src = rhs.chomp(ctx);
                    ctx.instructions.push(Instruction::Copy { src, dst });
                    Val::Var(dst)
                }
                ref otherwise => panic!("building tacky: got unexpected lhs: {otherwise:?}"),
            },
            Ternary {
                condition,
                true_case,
                false_case,
            } => todo!(),
        }
    }
}

pub struct TemporaryFactory<'a> {
    parent: &'a Identifier,
    tmp_i: usize,
    label_i: usize,

    // TODO: re-evaluate whether this is a good choice, or if we really should just go for stringly-typed temporaries after all.
    ident_to_i: HashMap<String, Temporary>,
}
impl<'a> TemporaryFactory<'a> {
    pub fn new(parent: &'a Identifier) -> Self {
        Self {
            parent,
            tmp_i: 0,
            label_i: 0,
            ident_to_i: HashMap::new(),
        }
    }

    pub fn next(&mut self) -> Temporary {
        let out = Temporary(self.tmp_i);
        self.tmp_i += 1;
        out
    }

    pub fn next_label(&mut self, kind: &str) -> Identifier {
        let out = format!("__{}.{}{}", self.parent.0, kind, self.label_i);
        self.label_i += 1;
        Identifier(out)
    }

    pub fn var(&mut self, ident: &str) -> Temporary {
        if let Some(existing) = self.ident_to_i.get(ident) {
            return *existing;
        }

        let out = self.next();
        self.ident_to_i.insert(ident.to_string(), out);
        out
    }
}
