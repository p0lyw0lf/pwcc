use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

mod codegen;
mod emitter;
mod lexer;
mod parser;
mod printer;
mod tacky;

fn print_help() {
    println!("{{--lex,--parse,--tacky,--codegen}} <file.i>");
}

fn main() -> Result<(), ()> {
    let mut lex = false;
    let mut parse = false;
    let mut tacky = false;
    let mut codegen = false;
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
            "--lex" => {
                lex = true;
            }
            "--parse" => {
                lex = true;
                parse = true;
            }
            "--tacky" => {
                lex = true;
                parse = true;
                tacky = true;
            }
            "--codegen" => {
                lex = true;
                parse = true;
                tacky = true;
                codegen = true;
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

    let output = !lex && !parse && !tacky && !codegen;
    if output {
        lex = true;
        parse = true;
        tacky = true;
        codegen = true;
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

    if !lex {
        return Ok(());
    }

    let tokens = lexer::lex(&source).map_err(|e| {
        eprintln!("error lexing: {e}");
    })?;

    if !parse {
        println!("{tokens:?}");
        return Ok(());
    }

    let tree = parser::parse(tokens).map_err(|e| {
        eprintln!("error parsing: {e}");
    })?;

    if !tacky {
        printer::pretty_print(tree);
        return Ok(());
    }

    let ir = tacky::Program::from(tree);

    if !codegen {
        println!("{ir:?}");
        return Ok(());
    }

    let pass0 = codegen::Program::<codegen::pseudo::State>::from(ir);
    if !output {
        println!("{pass0:?}");
    }

    let pass1: codegen::Program<codegen::stack::Pass> = pass0.run_pass();
    if !output {
        println!("{pass1:?}");
    }

    let pass2: codegen::Program<codegen::hardware::Pass> = pass1.run_pass();

    if !output {
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
