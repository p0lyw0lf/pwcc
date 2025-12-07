use crate::lexer::lex;
use crate::parser::FromTokens;
use crate::parser::Program;

mod goto;
mod ident_resolution;
mod loop_labeling;
mod type_check;

fn parse(source: &str) -> Program {
    Program::from_tokens(&mut lex(source).expect("lex failed").into_iter()).expect("parse failed")
}
