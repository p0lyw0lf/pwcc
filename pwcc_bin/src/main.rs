use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

use pwcc::{lexer, parser, tacky, codegen, printer};
use functional::Functor;

static STAGES: &'static [&'static str] = &["lex", "parse", "tacky", "codegen"];

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

fn main() -> Result<(), ()> {
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
                        eprintln!("Unexpected option \"{}\"", option);
                        print_help();
                        return Err(());
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
                    eprintln!("Passed multiple filenames! {existing} and {other}");
                    print_help();
                    return Err(());
                }
            },
        };
    }

    let filename = match filename {
        None => {
            eprintln!("Must provide a filename!");
            print_help();
            return Err(());
        }
        Some(f) => f,
    };

    let source = fs::read_to_string(filename.clone()).map_err(|e| {
        eprintln!("error reading file: {e}");
    })?;

    if stage.is_some() {
        return Ok(());
    }

    let tokens = lexer::lex(&source).map_err(|e| {
        eprintln!("error lexing: {e}");
    })?;

    if stage.map_or(false, |stage| stage < 1) {
        println!("{tokens:?}");
        return Ok(());
    }

    let tree = parser::parse(tokens).map_err(|e| {
        eprintln!("error parsing: {e}");
    })?;

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

    let pass1 = Functor::<codegen::Location<codegen::stack::Pass>>::fmap(pass0, &mut codegen::stack::pass);
    if stage.is_some() {
        println!("{pass1:?}");
    }

    let pass2 = Functor::<codegen::Instructions<codegen::hardware::Pass>>::fmap(pass1, &mut codegen::hardware::pass);

    if stage.is_some() {
        println!("{pass2:?}");
        return Ok(());
    }

    let code = pass2;

    let mut output_path = PathBuf::from(filename);
    output_path.set_extension("s");

    let mut output = fs::File::create(output_path).map_err(|e| {
        eprintln!("error opening output file: {e}");
    })?;

    write!(output, "{code}").map_err(|e| {
        eprintln!("error writing to output file: {e}");
    })?;

    Ok(())
}
