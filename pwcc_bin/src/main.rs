use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::str::FromStr;

use miette::{diagnostic, IntoDiagnostic, Result, WrapErr};

use functional::Functor;
use pwcc::{codegen, lexer, parser, printer, semantic, tacky};

mod stages;
use crate::stages::print_help;
use crate::stages::Stages;
use crate::stages::Stages::*;

fn main() -> Result<()> {
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
                        return Err(diagnostic!("Unexpected option \"{}\"", option).into());
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
                    return Err(diagnostic!(
                        "Passed multiple filenames! {} and {}",
                        existing,
                        other
                    )
                    .into());
                }
            },
        };
    }

    let filename = match filename {
        None => {
            print_help();
            return Err(diagnostic!("Must provide a filename").into());
        }
        Some(f) => f,
    };

    let source = fs::read_to_string(filename.clone())
        .into_diagnostic()
        .wrap_err("Error reading file")?;

    let tokens = lexer::lex(&source)
        .into_diagnostic()
        .wrap_err("Error lexing")?;

    if stage == Lex {
        println!("{tokens:?}");
        return Ok(());
    }

    let tree = parser::parse(tokens).wrap_err("Error parsing")?;

    if stage == Parse {
        printer::pretty_print(tree);
        return Ok(());
    }

    let tree = semantic::validate(tree)?;

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

    let pass1 = pass0.fmap(&mut codegen::stack::pass);
    if stage == Codegen {
        println!("{pass1:?}");
    }

    let pass2 = pass1.fmap(&mut codegen::hardware::pass);

    if stage == Codegen {
        println!("{pass2:?}");
        return Ok(());
    }

    let code = pass2;

    let mut output_path = PathBuf::from(filename);
    output_path.set_extension("s");

    let mut output = fs::File::create(output_path)
        .into_diagnostic()
        .wrap_err("Error opening output file")?;

    write!(output, "{code}")
        .into_diagnostic()
        .wrap_err("Error writing to output file")?;

    Ok(())
}
