use core::convert::From;

use crate::parser;

#[derive(Debug)]
pub struct Program(pub Function);

impl From<parser::Program> for Program {
    fn from(program: parser::Program) -> Self {
        Self(program.0.into())
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: parser::Identifier,
    pub instructions: Instructions,
}

impl From<parser::Function> for Function {
    fn from(function: parser::Function) -> Self {
        Self {
            name: function.0,
            instructions: function.1.into(),
        }
    }
}

#[derive(Debug)]
pub struct Instructions(pub Vec<Instruction>);

#[derive(Debug)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

impl From<parser::Statement> for Instructions {
    fn from(statement: parser::Statement) -> Self {
        Self(Vec::from([
            Instruction::Mov {
                src: statement.0.into(),
                dst: Operand::Register,
            },
            Instruction::Ret,
        ]))
    }
}

#[derive(Debug)]
pub enum Operand {
    Imm(isize),
    Register,
}

impl From<parser::Exp> for Operand {
    fn from(exp: parser::Exp) -> Self {
        Self::Imm(exp.0 .0)
    }
}