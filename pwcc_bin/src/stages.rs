use std::fmt::Display;
use std::str::FromStr;

macro_rules! stages {
($($name:ident = $val:literal ,)*) => {

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Stages {
    $($name,)*
    NoExplicitStage,
}
use Stages::*;

impl FromStr for Stages {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            $($val => Ok($name),)*
            _ => Err(()),
        }
    }
}

impl Display for Stages {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            $($name => write!(f, $val),)*
            NoExplicitStage => panic!("should not display NoExplicitStage"),
        }
    }
}

pub fn print_help() {
    println!(
        "{{{}}} <file.i>",
        [$($name,)*]
            .iter()
            .map(|stage| format!("--{}", stage))
            .collect::<Vec<_>>()
            .join(", ")
    );
}
};
}

stages! {
    Lex = "lex",
    Parse = "parse",
    Validate = "validate",
    Tacky = "tacky",
    Codegen = "codegen",
}
