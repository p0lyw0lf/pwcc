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
                Ampersand => write!(f, "&"),
                Caret => write!(f, "^"),
                CloseBrace => write!(f, "}}"),
                CloseParen => write!(f, ")"),
                Constant(n) => write!(f, "{n}"),
                Decrement => write!(f, "--"),
                DoubleAmpersand => write!(f, "&&"),
                DoubleEqual => write!(f, "=="),
                DoublePipe => write!(f, "||"),
                Equal => write!(f, "="),
                Exclamation => write!(f, "!"),
                ForwardSlash => write!(f, "/"),
                GreaterThan => write!(f, ">"),
                GreaterThanEqual => write!(f, ">="),
                Ident(i) => write!(f, "{i}"),
                Increment => write!(f, "++"),
                KeywordInt => write!(f, "int "),
                KeywordReturn => write!(f, "return "),
                KeywordVoid => write!(f, "void "),
                LeftShift => write!(f, "<<"),
                LessThan => write!(f, "<"),
                LessThanEqual => write!(f, "<="),
                Minus => write!(f, "-"),
                NotEqual => write!(f, "!="),
                OpenBrace => write!(f, " {{"),
                OpenParen => write!(f, "("),
                Percent => write!(f, "%"),
                Pipe => write!(f, "|"),
                Plus => write!(f, "+"),
                RightShift => write!(f, ">>"),
                Semicolon => write!(f, ";"),
                Star => write!(f, "*"),
                Tilde => write!(f, "~"),
            }
        } else {
            // Used for error output
            match self {
                KeywordInt => write!(f, "\"int\""),
                KeywordVoid => write!(f, "\"void\""),
                KeywordReturn => write!(f, "\"return\""),
                Ident(_) if f.sign_minus() => write!(f, "identifier"),
                Constant(_) if f.sign_minus() => write!(f, "constant"),
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
            Semicolon => {
                newline(false, false);
            }
            _ => {}
        };
    }

    println!();
}
