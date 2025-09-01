use core::convert::From;
use std::collections::HashMap;

use crate::parser;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

impl From<parser::Program> for Program {
    fn from(program: parser::Program) -> Self {
        Self {
            functions: program
                .functions
                .into_iter()
                .filter_map(|decl| Function::try_from(decl).ok())
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Identifier(pub String);

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub args: Vec<Temporary>,
    pub body: Instructions,
}

impl TryFrom<parser::FunctionDecl> for Function {
    type Error = ();
    fn try_from(function: parser::FunctionDecl) -> Result<Self, Self::Error> {
        let body = match function.body {
            parser::FunctionBody::Block(block) => block,
            parser::FunctionBody::Semicolon(_) => return Err(()),
        };

        let name = Identifier(function.name.0);
        let mut tf = TemporaryFactory::new(&name);
        let args = function
            .args
            .into_iter()
            .map(|arg| tf.var(&arg.name.0))
            .collect::<Vec<_>>();

        Ok(Self {
            body: Instructions::from_function(tf, body),
            name,
            args,
        })
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
    Call {
        name: Identifier,
        args: Vec<Val>,
        dst: Temporary,
    },
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

impl<'a> ChompContext<'a> {
    fn push(&mut self, i: Instruction) {
        self.instructions.push(i)
    }

    fn goto_label(&self, label: &str) -> Identifier {
        Identifier(format!("__{}.goto.{}", self.tf.parent, label))
    }

    fn break_label(&self, loop_label: &parser::LoopLabel) -> Identifier {
        Identifier(format!("__break.{}", loop_label.0))
    }

    fn continue_label(&self, loop_label: &parser::LoopLabel) -> Identifier {
        Identifier(format!("__continue.{}", loop_label.0))
    }

    fn case_label(&self, case_label: &str) -> Identifier {
        Identifier(format!("__case.{}", case_label))
    }
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
                let dst = ctx.tf.next_temp();
                ctx.push(Instruction::Copy { src, dst });
                dst
            }
        }
    }
}

impl Instructions {
    fn from_function(tf: TemporaryFactory, block: parser::Block) -> Self {
        let instructions = Vec::<Instruction>::new();
        let mut ctx = ChompContext { instructions, tf };
        block.items.chomp(&mut ctx);

        Self(ctx.instructions)
    }
}

impl<T> Chompable for Box<T>
where
    T: Chompable,
{
    type Output = T::Output;
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output {
        (*self).chomp(ctx)
    }
}

impl<T> Chompable for Option<T>
where
    T: Chompable<Output = ()>,
{
    type Output = ();
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output {
        self.map(|v| v.chomp(ctx)).unwrap_or_default()
    }
}

impl<T> Chompable for Vec<T>
where
    T: Chompable<Output = ()>,
{
    type Output = ();
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output {
        for t in self {
            t.chomp(ctx);
        }
    }
}

impl Chompable for parser::BlockItem {
    type Output = ();
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output {
        use parser::BlockItem::*;
        use parser::Declaration::*;
        match self {
            Declaration(VarDecl(decl)) => decl.chomp(ctx),
            Declaration(FunctionDecl(decl)) => decl.chomp(ctx),
            Statement(statement) => statement.chomp(ctx),
        }
    }
}

impl Chompable for parser::VarDecl {
    type Output = ();
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output {
        use parser::Initializer::*;
        match self.init {
            NoInit(_) => {}
            ExpressionInit(expression_init) => {
                let src = expression_init.exp.chomp(ctx);
                let dst = ctx.tf.var(&self.name.0);
                ctx.push(Instruction::Copy { src, dst });
            }
        }
    }
}

impl Chompable for parser::FunctionDecl {
    type Output = ();
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Self::Output {
        use parser::FunctionBody::*;
        match self.body {
            Semicolon(_) => {}
            Block(block) => {
                block.items.chomp(ctx);

                // See page 113; This will do nothing if the function is well-formed, and guarantees we
                // exit in case there is undefined behavior.
                ctx.push(Instruction::Return {
                    val: Val::Constant(0),
                });
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
            Block(block) => block.items.chomp(ctx),
            ReturnStmt(return_statement) => {
                let val = return_statement.exp.chomp(ctx);
                ctx.push(Instruction::Return { val });
            }
            ExpressionStmt(expression_stmt) => {
                let _ = expression_stmt.exp.chomp(ctx);
            }
            LabelStmt(parser::LabelStmt {
                label,
                stmt,
                span: _span,
            }) => {
                match label {
                    parser::Label::RawLabel(parser::RawLabel { label, span: _span }) => {
                        ctx.push(Instruction::Label(ctx.goto_label(&label.0)));
                    }
                    parser::Label::CaseLabel(case_label) => match case_label {
                        parser::CaseLabel::Labeled(label, _) => {
                            ctx.push(Instruction::Label(ctx.case_label(&label)));
                        }
                        otherwise => panic!("encountered unprocessed case label {otherwise:?}"),
                    },
                };
                stmt.chomp(ctx);
            }
            GotoStmt(parser::GotoStmt { label, span: _span }) => {
                ctx.push(Instruction::Jump {
                    target: ctx.goto_label(&label.0),
                });
            }
            IfStmt(parser::IfStmt {
                guard,
                body,
                else_stmt: None,
                span: _span,
            }) => {
                let condition = guard.chomp(ctx).chomp(ctx);
                let end = ctx.tf.next_label("end");
                ctx.push(Instruction::JumpIfZero {
                    condition,
                    target: end.clone(),
                });
                body.chomp(ctx);
                ctx.push(Instruction::Label(end));
            }
            IfStmt(parser::IfStmt {
                guard,
                body,
                else_stmt: Some(else_stmt),
                span: _span,
            }) => {
                let condition = guard.chomp(ctx).chomp(ctx);
                let else_label = ctx.tf.next_label("else");
                let end = ctx.tf.next_label("end");
                ctx.push(Instruction::JumpIfZero {
                    condition,
                    target: else_label.clone(),
                });
                body.chomp(ctx);
                ctx.push(Instruction::Jump {
                    target: end.clone(),
                });
                ctx.push(Instruction::Label(else_label));
                else_stmt.body.chomp(ctx);
                ctx.push(Instruction::Label(end));
            }
            SwitchStmt(switch_stmt) => {
                let break_label = ctx.break_label(
                    switch_stmt
                        .label
                        .as_ref()
                        .expect("switch statement without loop label"),
                );
                let lhs = switch_stmt.exp.chomp(ctx).chomp(ctx);

                let mut labels = switch_stmt.ctx.expect("switch satement without context").0;
                let default_case = labels.remove(&None);

                // Check all the present cases, in order
                for (check, label) in labels {
                    if let Some(rhs) = check {
                        let condition = ctx.tf.next_temp();
                        ctx.push(Instruction::Binary {
                            src1: Val::Var(lhs),
                            op: BinaryOp::Equal,
                            src2: Val::Constant(rhs),
                            dst: condition,
                        });
                        ctx.push(Instruction::JumpIfNotZero {
                            condition,
                            target: ctx.case_label(&label.0),
                        });
                    }
                }

                // Then, emit the default case, if present
                match default_case {
                    Some(label) => ctx.push(Instruction::Jump {
                        target: ctx.case_label(&label.0),
                    }),
                    // Otherwise, jump immediately to the end.
                    None => ctx.push(Instruction::Jump {
                        target: break_label.clone(),
                    }),
                };

                switch_stmt.body.chomp(ctx);

                ctx.push(Instruction::Label(break_label));
            }
            BreakStmt(break_stmt) => {
                ctx.push(Instruction::Jump {
                    target: ctx.break_label(
                        break_stmt
                            .label
                            .as_ref()
                            .expect("break statement did not have loop label"),
                    ),
                });
            }
            ContinueStmt(continue_stmt) => {
                ctx.push(Instruction::Jump {
                    target: ctx.continue_label(
                        continue_stmt
                            .label
                            .as_ref()
                            .expect("continue statement did not have loop label"),
                    ),
                });
            }
            DoWhileStmt(do_while_stmt) => {
                let loop_label = do_while_stmt
                    .label
                    .as_ref()
                    .expect("do while statement did not have loop label");
                let start = ctx.tf.next_label("start");
                ctx.push(Instruction::Label(start.clone()));
                do_while_stmt.body.chomp(ctx);
                ctx.push(Instruction::Label(ctx.continue_label(loop_label)));
                let condition = do_while_stmt.guard.chomp(ctx).chomp(ctx);
                ctx.push(Instruction::JumpIfNotZero {
                    condition,
                    target: start,
                });
                ctx.push(Instruction::Label(ctx.break_label(loop_label)));
            }
            WhileStmt(while_stmt) => {
                let loop_label = while_stmt
                    .label
                    .as_ref()
                    .expect("while statement did not have loop label");
                let continue_label = ctx.continue_label(loop_label);
                let break_label = ctx.break_label(loop_label);
                ctx.push(Instruction::Label(continue_label.clone()));
                let condition = while_stmt.guard.chomp(ctx).chomp(ctx);
                ctx.push(Instruction::JumpIfZero {
                    condition,
                    target: break_label.clone(),
                });
                while_stmt.body.chomp(ctx);
                ctx.push(Instruction::Jump {
                    target: continue_label,
                });
                ctx.push(Instruction::Label(break_label));
            }
            ForStmt(for_stmt) => {
                match for_stmt.init {
                    parser::ForInit::VarDecl(decl) => decl.chomp(ctx),
                    parser::ForInit::ForInitExp(parser::ForInitExp { exp, span: _span }) => {
                        exp.map(|exp| exp.chomp(ctx));
                    }
                };
                let start = ctx.tf.next_label("start");
                let loop_label = for_stmt
                    .label
                    .as_ref()
                    .expect("for statement did not have loop label");
                let continue_label = ctx.continue_label(loop_label);
                let break_label = ctx.break_label(loop_label);
                ctx.push(Instruction::Label(start.clone()));
                if let Some(guard) = for_stmt.exp1 {
                    let condition = guard.chomp(ctx).chomp(ctx);
                    ctx.push(Instruction::JumpIfZero {
                        condition,
                        target: break_label.clone(),
                    });
                }
                for_stmt.body.chomp(ctx);
                ctx.push(Instruction::Label(continue_label));
                if let Some(post) = for_stmt.exp2 {
                    post.chomp(ctx);
                }
                ctx.push(Instruction::Jump { target: start });
                ctx.push(Instruction::Label(break_label));
            }
        }
    }
}

/// Takes in an expression, list of instructions, and the next temporary available, and returns the
/// final value of the expression.
impl Chompable for parser::Exp {
    type Output = Val;
    fn chomp<'a>(self, ctx: &mut ChompContext<'a>) -> Val {
        use Instruction::*;
        match self {
            parser::Exp::Constant {
                constant,
                span: _span,
            } => Val::Constant(constant),
            parser::Exp::Unary {
                op:
                    parser::UnaryOp::PrefixOp(
                        op @ (parser::PrefixOp::Minus(_)
                        | parser::PrefixOp::Tilde(_)
                        | parser::PrefixOp::Exclamation(_)),
                    ),
                exp,
                span: _span,
            } => {
                let src = exp.chomp(ctx);
                let dst = ctx.tf.next_temp();
                use parser::PrefixOp::*;
                let op = match op {
                    Minus(_) => UnaryOp::Negate,
                    Tilde(_) => UnaryOp::Complement,
                    Exclamation(_) => UnaryOp::Not,
                    _ => unreachable!(),
                };
                ctx.push(Unary { op, src, dst });

                Val::Var(dst)
            }
            parser::Exp::Unary {
                op:
                    parser::UnaryOp::PrefixOp(
                        op @ (parser::PrefixOp::Increment(_) | parser::PrefixOp::Decrement(_)),
                    ),
                exp,
                span: _span,
            } => match *exp {
                parser::Exp::Var { ident, .. } => {
                    let dst = ctx.tf.var(&ident);
                    let src = Val::Var(dst);

                    use parser::PrefixOp::*;
                    ctx.push(Binary {
                        src1: src,
                        op: match op {
                            Increment(_) => BinaryOp::Add,
                            Decrement(_) => BinaryOp::Subtract,
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
            parser::Exp::Unary {
                op: parser::UnaryOp::PostfixOp(op),
                exp,
                span: _span,
            } => match *exp {
                parser::Exp::Var { ident, .. } => {
                    let dst = ctx.tf.var(&ident);
                    let src = Val::Var(dst);
                    let tmp = ctx.tf.next_temp();

                    ctx.push(Copy { src, dst: tmp });

                    let src = Val::Var(dst);
                    use parser::PostfixOp::*;
                    ctx.push(Binary {
                        src1: src,
                        op: match op {
                            Increment(_) => BinaryOp::Add,
                            Decrement(_) => BinaryOp::Subtract,
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
            parser::Exp::Binary {
                lhs,
                op: op @ (parser::BinaryOp::DoubleAmpersand(_) | parser::BinaryOp::DoublePipe(_)),
                rhs,
                span: _span,
            } => {
                let is_and = matches!(op, parser::BinaryOp::DoubleAmpersand(_));

                let shortcut_label = ctx.tf.next_label(if is_and { "false" } else { "true" });

                let src1 = lhs.chomp(ctx).chomp(ctx);
                ctx.push(if is_and {
                    JumpIfZero {
                        condition: src1,
                        target: shortcut_label.clone(),
                    }
                } else {
                    JumpIfNotZero {
                        condition: src1,
                        target: shortcut_label.clone(),
                    }
                });

                let src2 = rhs.chomp(ctx).chomp(ctx);
                ctx.push(if is_and {
                    JumpIfZero {
                        condition: src2,
                        target: shortcut_label.clone(),
                    }
                } else {
                    JumpIfNotZero {
                        condition: src2,
                        target: shortcut_label.clone(),
                    }
                });

                let dst = ctx.tf.next_temp();
                let end_label = ctx.tf.next_label("end");
                ctx.instructions.extend([
                    Copy {
                        src: Val::Constant(if is_and { 1 } else { 0 }),
                        dst,
                    },
                    Jump {
                        target: end_label.clone(),
                    },
                    Label(shortcut_label),
                    Copy {
                        src: Val::Constant(if is_and { 0 } else { 1 }),
                        dst,
                    },
                    Label(end_label),
                ]);

                Val::Var(dst)
            }
            parser::Exp::Binary {
                lhs,
                op,
                rhs,
                span: _span,
            } => {
                let src1 = lhs.chomp(ctx);
                let src2 = rhs.chomp(ctx);
                let dst = ctx.tf.next_temp();
                use parser::BinaryOp::*;
                let op = match op {
                    Ampersand(_) => BinaryOp::BitAnd,
                    Caret(_) => BinaryOp::BitXor,
                    DoubleAmpersand(_) => unreachable!(),
                    DoubleEqual(_) => BinaryOp::Equal,
                    DoublePipe(_) => unreachable!(),
                    ForwardSlash(_) => BinaryOp::Divide,
                    GreaterThan(_) => BinaryOp::GreaterThan,
                    GreaterThanEqual(_) => BinaryOp::GreaterThanEqual,
                    LeftShift(_) => BinaryOp::BitLeftShift,
                    LessThan(_) => BinaryOp::LessThan,
                    LessThanEqual(_) => BinaryOp::LessThanEqual,
                    Minus(_) => BinaryOp::Subtract,
                    NotEqual(_) => BinaryOp::NotEqual,
                    Percent(_) => BinaryOp::Remainder,
                    Pipe(_) => BinaryOp::BitOr,
                    Plus(_) => BinaryOp::Add,
                    RightShift(_) => BinaryOp::BitRightShift,
                    Star(_) => BinaryOp::Multiply,
                };
                ctx.push(Binary {
                    op,
                    src1,
                    src2,
                    dst,
                });

                Val::Var(dst)
            }
            parser::Exp::Var { ident, span: _span } => Val::Var(ctx.tf.var(&ident)),
            parser::Exp::Assignment { lhs, op, rhs, span } => match *lhs {
                parser::Exp::Var { ref ident, .. } => {
                    let dst = ctx.tf.var(ident);
                    let rhs = match op {
                        parser::AssignmentOp::Equal(_) => *rhs,
                        otherwise => parser::Exp::Binary {
                            lhs,
                            op: otherwise
                                .to_binary_op()
                                .expect("to convert AssignmentOp to BinaryOp"),
                            rhs,
                            span,
                        },
                    };
                    let src = rhs.chomp(ctx);
                    ctx.push(Copy { src, dst });
                    Val::Var(dst)
                }
                ref otherwise => panic!("building tacky: got unexpected lhs: {otherwise:?}"),
            },
            parser::Exp::Ternary {
                condition,
                true_case,
                false_case,
                span: _span,
            } => {
                let condition = condition.chomp(ctx).chomp(ctx);
                let result = ctx.tf.next_temp();
                let else_label = ctx.tf.next_label("else");
                let end = ctx.tf.next_label("end");

                ctx.push(JumpIfZero {
                    condition,
                    target: else_label.clone(),
                });
                let true_val = true_case.chomp(ctx);
                ctx.push(Copy {
                    src: true_val,
                    dst: result,
                });
                ctx.push(Jump {
                    target: end.clone(),
                });
                ctx.push(Label(else_label));
                let false_val = false_case.chomp(ctx);
                ctx.push(Copy {
                    src: false_val,
                    dst: result,
                });
                ctx.push(Label(end));

                Val::Var(result)
            }
            parser::Exp::FunctionCall {
                ident,
                args,
                span: _span,
            } => {
                let vals = args
                    .0
                    .into_iter()
                    .map(|arg| arg.chomp(ctx))
                    .collect::<Vec<_>>();
                let dst = ctx.tf.next_temp();
                ctx.push(Call {
                    name: Identifier(ident.0),
                    args: vals,
                    dst,
                });

                Val::Var(dst)
            }
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

    pub fn next_temp(&mut self) -> Temporary {
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

        let out = self.next_temp();
        self.ident_to_i.insert(ident.to_string(), out);
        out
    }
}
