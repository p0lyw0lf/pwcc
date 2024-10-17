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

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Instructions,
}

impl From<parser::Function> for Function {
    fn from(function: parser::Function) -> Self {
        Self {
            name: function.name,
            body: function.statement.into(),
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
}

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate,
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
}

impl From<parser::Statement> for Instructions {
    fn from(statement: parser::Statement) -> Self {
        let mut instructions = Vec::<Instruction>::new();
        let last_val = chomp_exp(
            statement.exp,
            &mut instructions,
            &mut TemporaryFactory::default(),
        );
        instructions.push(Instruction::Return { val: last_val });
        Self(instructions)
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
                Exclamation => todo!(),
                Minus => UnaryOp::Negate,
                Tilde => UnaryOp::Complement,
            };
            instructions.push(Instruction::Unary { op, src, dst });

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
                DoubleAmpersand => todo!(),
                DoubleEqual => todo!(),
                DoublePipe => todo!(),
                ForwardSlash => BinaryOp::Divide,
                GreaterThan => todo!(),
                GreaterThanEqual => todo!(),
                LeftShift => BinaryOp::BitLeftShift,
                LessThan => todo!(),
                LessThanEqual => todo!(),
                Minus => BinaryOp::Subtract,
                NotEqual => todo!(),
                Percent => BinaryOp::Remainder,
                Pipe => BinaryOp::BitOr,
                Plus => BinaryOp::Add,
                RightShift => BinaryOp::BitRightShift,
                Star => BinaryOp::Multiply,
            };
            instructions.push(Instruction::Binary { op, src1, src2, dst });

            Val::Var(dst)
        }
    }
}

#[derive(Debug)]
pub enum Val {
    Constant(isize),
    Var(Temporary),
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Temporary(pub usize);

#[derive(Default)]
pub struct TemporaryFactory(usize);
impl TemporaryFactory {
    pub fn next(&mut self) -> Temporary {
        let out = Temporary(self.0);
        self.0 += 1;
        out
    }
}
