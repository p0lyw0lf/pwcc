use std::env;
use std::fmt::Display;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::str::FromStr;

use functional::Functor;
use pwcc::{codegen, lexer, parser, printer, semantic, tacky};

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum Stages {
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
    NoExplicitStage,
}
use Stages::*;

impl FromStr for Stages {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lex" => Ok(Lex),
            "parse" => Ok(Parse),
            "validate" => Ok(Validate),
            "tacky" => Ok(Tacky),
            "codegen" => Ok(Codegen),
            _ => Err(()),
        }
    }
}

impl Display for Stages {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lex => write!(f, "lex"),
            Parse => write!(f, "parse"),
            Validate => write!(f, "validate"),
            Tacky => write!(f, "tacky"),
            Codegen => write!(f, "codegen"),
            NoExplicitStage => panic!("should not display NoExplicitStage"),
        }
    }
}

fn print_help() {
    println!(
        "{{{}}} <file.i>",
        [Lex, Parse, Validate, Tacky, Codegen]
            .iter()
            .map(|stage| format!("--{}", stage))
            .collect::<Vec<_>>()
            .join(", ")
    );
}

fn main() -> Result<(), String> {
    let mut stage = NoExplicitStage;
    let mut filename: Option<String> = None;

    for arg in env::args().skip(1) {
        match arg.as_str() {
            "-h" | "--help" => {
                print_help();
                return Ok(());
            }
            "-v" | "--version" => {
                println!("{} v{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
                return Ok(());
            }
            option if option.starts_with("--") => {
                let stage_str = option.strip_prefix("--").unwrap();
                match Stages::from_str(stage_str) {
                    Err(()) => {
                        print_help();
                        return Err(format!("Unexpected option \"{option}\""));
                    }
                    Ok(new_stage) => {
                        if stage == NoExplicitStage {
                            stage = new_stage;
                        } else {
                            stage = std::cmp::max(stage, new_stage);
                        }
                    }
                }
            }
            other => match filename {
                None => filename = Some(other.to_string()),
                Some(existing) => {
                    print_help();
                    return Err(format!("Passed multiple filenames! {existing} and {other}"));
                }
            },
        };
    }

    let filename = match filename {
        None => {
            print_help();
            return Err("Must provide a filename".into());
        }
        Some(f) => f,
    };

    let source =
        fs::read_to_string(filename.clone()).map_err(|e| format!("error reading file: {e}"))?;

    let tokens = lexer::lex(&source).map_err(|e| format!("error lexing: {e}"))?;

    if stage == Lex {
        println!("{tokens:?}");
        return Ok(());
    }

    let tree = parser::parse(tokens).map_err(|e| format!("error parsing: {e}"))?;

    if stage == Parse {
        printer::pretty_print(tree);
        return Ok(());
    }

    let tree =
        semantic::validate(tree).map_err(|e| format!("error running semantic analysis: {e}"))?;

    if stage == Validate {
        printer::pretty_print(tree);
        return Ok(());
    }

    let ir = tacky::Program::from(tree);

    if stage == Tacky {
        println!("{ir:?}");
        return Ok(());
    }

    let pass0 = codegen::Program::<codegen::pseudo::State>::from(ir);
    if stage == Codegen {
        println!("{pass0:?}");
    }

    let pass1 = Functor::<codegen::Location<_>>::fmap(pass0, &mut codegen::stack::pass);
    if stage == Codegen {
        println!("{pass1:?}");
    }

    let pass2 = Functor::<codegen::Instructions<_>>::fmap(pass1, &mut codegen::hardware::pass);

    if stage == Codegen {
        println!("{pass2:?}");
        return Ok(());
    }

    let code = pass2;

    let mut output_path = PathBuf::from(filename);
    output_path.set_extension("s");

    let mut output =
        fs::File::create(output_path).map_err(|e| format!("error opening output file: {e}"))?;

    write!(output, "{code}").map_err(|e| format!("error writing to output file: {e}"))?;

    Ok(())
}
