use std::env;
use std::fs;

mod lexer;
mod parser;
mod printer;

fn print_help() {
    println!("{{--lex,--parse,--codegen,}} <file.i>");
}

fn main() -> Result<(), ()> {
    let mut lex = false;
    let mut parse = false;
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
            "--codegen" => {
                lex = true;
                parse = true;
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

    let output = !lex && !parse && !codegen;
    if output {
        lex = true;
        parse = true;
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

    let source = fs::read_to_string(filename).map_err(|e| {
        eprintln!("error reading file: {e}");
    })?;

    if !lex {
        return Ok(());
    }

    let tokens = lexer::lex(&source).map_err(|e| {
        eprintln!("error lexing: {e}");
    })?;
    println!("{tokens:?}");

    if !parse {
        return Ok(());
    }

    let tree = parser::parse(tokens).map_err(|e| {
        eprintln!("error parsing: {e}");
    })?;
    println!("{tree:?}");
    printer::pretty_print(tree);

    if !codegen {
        return Ok(());
    }

    println!("codegen");

    if !output {
        return Ok(());
    }

    println!("output");

    Ok(())
}
