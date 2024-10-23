use core::convert::From;

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

// TODO: split this between "real" (in source code) and "fake" (made up by the compiler)
// identifiers?
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

impl Instructions {
    fn from_function(identifier: &Identifier, statement: parser::Body) -> Self {
        let mut instructions = Vec::<Instruction>::new();
        todo!();
        Self(instructions)
    }
}

fn make_temporary(
    v: Val,
    instructions: &mut Vec<Instruction>,
    tf: &mut TemporaryFactory,
) -> Temporary {
    match v {
        Val::Var(t) => t,
        c @ Val::Constant(_) => {
            let dst = tf.next();
            instructions.push(Instruction::Copy {
                src: c,
                dst: dst.clone(),
            });
            dst
        }
    }
}

/// Takes in an expression, list of instructions, and the next temporary available, and returns the
/// final value of the expression.
fn chomp_exp(
    exp: parser::Exp,
    instructions: &mut Vec<Instruction>,
    tf: &mut TemporaryFactory,
) -> Val {
    use parser::Exp::*;
    match exp {
        Constant { constant } => Val::Constant(constant),
        Unary { op, exp } => {
            let src = chomp_exp(*exp, instructions, tf);
            let dst = tf.next();
            use parser::UnaryOp::*;
            let op = match op {
                Exclamation => UnaryOp::Not,
                Minus => UnaryOp::Negate,
                Tilde => UnaryOp::Complement,
            };
            instructions.push(Instruction::Unary { op, src, dst });

            Val::Var(dst)
        }
        Binary {
            lhs,
            op: op @ (parser::BinaryOp::DoubleAmpersand | parser::BinaryOp::DoublePipe),
            rhs,
        } => {
            let is_and = matches!(op, parser::BinaryOp::DoubleAmpersand);

            let shortcut_label = tf.next_label(if is_and { "false" } else { "true" });

            let src1 = make_temporary(chomp_exp(*lhs, instructions, tf), instructions, tf);
            instructions.push(if is_and {
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

            let src2 = make_temporary(chomp_exp(*rhs, instructions, tf), instructions, tf);
            instructions.push(if is_and {
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

            let dst = tf.next();
            let end_label = tf.next_label("end");
            instructions.extend(
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
            let src1 = chomp_exp(*lhs, instructions, tf);
            let src2 = chomp_exp(*rhs, instructions, tf);
            let dst = tf.next();
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
            instructions.push(Instruction::Binary {
                op,
                src1,
                src2,
                dst,
            });

            Val::Var(dst)
        }
        Var { ident } => todo!(),
        Assignment { lhs, rhs } => todo!(),
    }
}

#[derive(Debug)]
pub enum Val {
    Constant(isize),
    Var(Temporary),
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Temporary(pub usize);

pub struct TemporaryFactory<'a> {
    parent: &'a Identifier,
    tmp_i: usize,
    label_i: usize,
}
impl<'a> TemporaryFactory<'a> {
    pub fn new(parent: &'a Identifier) -> Self {
        Self { parent, tmp_i: 0, label_i: 0 }
    }
    pub fn next(&mut self) -> Temporary {
        let out = Temporary(self.tmp_i);
        self.tmp_i += 1;
        out
    }
    pub fn next_label(&mut self, kind: &str) -> Identifier {
        let out = format!("{}_{}{}", self.parent.0, kind, self.label_i);
        self.label_i += 1;
        Identifier(out)
    }
}
