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
                Ampersand => f.write_str("&"),
                Caret => f.write_str("^"),
                CloseBrace => f.write_str("}"),
                CloseParen => f.write_str(")"),
                Constant(n) => write!(f, "{n}"),
                Decrement => f.write_str("--"),
                DoubleAmpersand => f.write_str("&&"),
                DoubleEqual => f.write_str("=="),
                DoublePipe => f.write_str("||"),
                Exclamation => f.write_str("!"),
                ForwardSlash => f.write_str("/"),
                GreaterThan => f.write_str(">"),
                GreaterThanEqual => f.write_str(">="),
                Ident(i) => f.write_str(i),
                Increment => f.write_str("++"),
                KeywordInt => f.write_str("int "),
                KeywordReturn => f.write_str("return "),
                KeywordVoid => f.write_str("void "),
                LeftShift => f.write_str("<<"),
                LessThan => f.write_str("<"),
                LessThanEqual => f.write_str("<="),
                Minus => f.write_str("-"),
                NotEqual => f.write_str("!="),
                OpenBrace => f.write_str(" {"),
                OpenParen => f.write_str("("),
                Percent => f.write_str("%"),
                Pipe => f.write_str("|"),
                Plus => f.write_str("+"),
                RightShift => f.write_str(">>"),
                Semicolon => f.write_str(";"),
                Star => f.write_str("*"),
                Tilde => f.write_str("~"),
            }
        } else {
            // Used for error output
            match self {
                KeywordInt => f.write_str("\"int\""),
                KeywordVoid => f.write_str("\"void\""),
                KeywordReturn => f.write_str("\"return\""),
                Ident(_) if f.sign_minus() => f.write_str("identifier"),
                Constant(_) if f.sign_minus() => f.write_str("constant"),
                other => write!(f, "\"{other}\""),
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
