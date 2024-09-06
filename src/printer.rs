use std::fmt::Display;

use crate::lexer::Token;
use crate::parser::ToTokens;

mod errors;

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;

        if f.alternate() {
            // Used for pretty-printing
            match self {
                KeywordInt => f.write_str("int "),
                KeywordVoid => f.write_str("void"),
                KeywordReturn => f.write_str("return "),
                Ident(i) => f.write_str(i),
                Constant(n) => write!(f, "{n}"),
                OpenBrace => f.write_str(" {"),
                CloseBrace => f.write_str("}"),
                OpenParen => f.write_str("("),
                CloseParen => f.write_str(")"),
                Semicolon => f.write_str(";"),
            }
        } else {
            // Used for error output
            match self {
                KeywordInt => f.write_str(r#""int""#),
                KeywordVoid => f.write_str(r#""void""#),
                KeywordReturn => f.write_str(r#""return""#),
                Ident(i) => {
                    if f.sign_minus() {
                        f.write_str(r#"identifier"#)
                    } else {
                        write!(f, "\"{i}\"")
                    }
                }
                Constant(c) => {
                    if f.sign_minus() {
                        f.write_str(r#"constant"#)
                    } else {
                        write!(f, "\"{c}\"")
                    }
                }
                OpenBrace => f.write_str(r#""{""#),
                CloseBrace => f.write_str(r#""}""#),
                OpenParen => f.write_str(r#""(""#),
                CloseParen => f.write_str(r#"")""#),
                Semicolon => f.write_str(r#"";""#),
            }
        }
    }
}

pub fn pretty_print(tree: impl ToTokens) {
    use Token::*;

    let indentation = &mut 0usize;
    let mut newline = |inc: bool, dec: bool| {
        if inc {
            *indentation = indentation.saturating_add(1);
        }
        if dec {
            *indentation = indentation.saturating_sub(1);
        }

        println!();
        if *indentation > 0 {
            print!("{}", "\t".repeat(*indentation));
        }
    };

    for token in tree.to_tokens() {
        match token {
            CloseBrace => {
                newline(false, true);
            }
            _ => {}
        };

        print!("{token:#}");

        match token {
            OpenBrace => {
                newline(true, false);
            }
            _ => {}
        };
    }

    println!();
}
