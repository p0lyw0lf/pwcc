use pwcc_util::parser::{FromTokens, as_cloneable};

use crate::lexer::lex;
use crate::parser::Program;

mod goto;
mod ident_resolution;
mod loop_labeling;
mod storage_check;
mod type_check;

fn parse(source: &str) -> Program {
    let tokens = lex(source).expect("lex failed");
    let mut iter = as_cloneable(&tokens);
    Program::from_tokens(&mut iter).expect("parse failed")
}
