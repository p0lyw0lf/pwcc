use core::convert::From;

use crate::parser;

#[derive(Debug)]
pub struct Program(pub Function);

#[derive(Debug)]
pub struct Function {
    pub name: parser::Identifier,
    pub instructions: Instructions,
}

#[derive(Debug)]
pub struct Instructions(pub Vec<Instruction>);

#[derive(Debug)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

#[derive(Debug)]
pub enum Operand {
    Imm(isize),
    Register,
}
