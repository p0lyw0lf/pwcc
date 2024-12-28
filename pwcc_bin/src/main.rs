use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

use functional::Functor;
use pwcc::{codegen, lexer, parser, printer, semantic, tacky};

static STAGES: &'static [&'static str] = &["lex", "parse", "validate", "tacky", "codegen"];

fn print_help() {
    println!(
        "{{{}}} <file.i>",
        STAGES
            .iter()
            .map(|stage| format!("--{}", stage))
            .collect::<Vec<_>>()
            .join(", ")
    );
}

fn main() -> Result<(), String> {
    let mut stage: Option<usize> = None;
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
                match STAGES.iter().position(|s| s == &stage_str) {
                    None => {
                        print_help();
                        return Err(format!("Unexpected option \"{option}\""));
                    }
                    Some(new_stage) => {
                        if let Some(old_stage) = stage {
                            stage = Some(core::cmp::max(old_stage, new_stage));
                        } else {
                            stage = Some(new_stage);
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

    if stage.map_or(false, |stage| stage < 1) {
        println!("{tokens:?}");
        return Ok(());
    }

    let tree = parser::parse(tokens).map_err(|e| format!("error parsing: {e}"))?;
    let tree =
        semantic::validate(tree).map_err(|e| format!("error running semantic analysis: {e}"))?;

    if stage.map_or(false, |stage| stage < 2) {
        printer::pretty_print(tree);
        return Ok(());
    }

    let ir = tacky::Program::from(tree);

    if stage.map_or(false, |stage| stage < 3) {
        println!("{ir:?}");
        return Ok(());
    }

    let pass0 = codegen::Program::<codegen::pseudo::State>::from(ir);
    if stage.is_some() {
        println!("{pass0:?}");
    }

    let pass1 = Functor::<codegen::Location<_>>::fmap(pass0, &mut codegen::stack::pass);
    if stage.is_some() {
        println!("{pass1:?}");
    }

    let pass2 = Functor::<codegen::Instructions<_>>::fmap(pass1, &mut codegen::hardware::pass);

    if stage.is_some() {
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
