use std::fmt::Display;

use crate::lexer::Token;
use crate::parser::ToTokens;

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;

        if f.alternate() {
            // Used for pretty-printing
            match self {
                Ampersand => write!(f, "&"),
                AmpersandEqual => write!(f, "&="),
                Caret => write!(f, "^"),
                CaretEqual => write!(f, "^="),
                CloseBrace => write!(f, "}}"),
                CloseParen => write!(f, ")"),
                Colon => write!(f, ":"),
                Constant(n) => write!(f, "{n}"),
                Decrement => write!(f, "--"),
                DoubleAmpersand => write!(f, "&&"),
                DoubleEqual => write!(f, "=="),
                DoublePipe => write!(f, "||"),
                Equal => write!(f, "="),
                Exclamation => write!(f, "!"),
                ForwardSlash => write!(f, "/"),
                ForwardSlashEqual => write!(f, "/="),
                GreaterThan => write!(f, ">"),
                GreaterThanEqual => write!(f, ">="),
                Ident(i) => write!(f, "{i}"),
                Increment => write!(f, "++"),
                KeywordElse => write!(f, "else "),
                KeywordGoto => write!(f, "goto"),
                KeywordIf => write!(f, "if "),
                KeywordInt => write!(f, "int "),
                KeywordReturn => write!(f, "return "),
                KeywordVoid => write!(f, "void "),
                LeftShift => write!(f, "<<"),
                LeftShiftEqual => write!(f, "<<="),
                LessThan => write!(f, "<"),
                LessThanEqual => write!(f, "<="),
                Minus => write!(f, "-"),
                MinusEqual => write!(f, "-="),
                NotEqual => write!(f, "!="),
                OpenBrace => write!(f, " {{"),
                OpenParen => write!(f, "("),
                Percent => write!(f, "%"),
                PercentEqual => write!(f, "%="),
                Pipe => write!(f, "|"),
                PipeEqual => write!(f, "|="),
                Plus => write!(f, "+"),
                PlusEqual => write!(f, "+="),
                Question => write!(f, "?"),
                RightShift => write!(f, ">>"),
                RightShiftEqual => write!(f, ">>="),
                Semicolon => write!(f, ";"),
                Star => write!(f, "*"),
                StarEqual => write!(f, "*="),
                Tilde => write!(f, "~"),
            }
        } else {
            // Used for error output
            match self {
                Constant(_) if f.sign_minus() => write!(f, "constant"),
                Ident(_) if f.sign_minus() => write!(f, "identifier"),
                KeywordElse => write!(f, "\"else\""),
                KeywordGoto => write!(f, "\"goto\""),
                KeywordIf => write!(f, "\"if\""),
                KeywordInt => write!(f, "\"int\""),
                KeywordReturn => write!(f, "\"return\""),
                KeywordVoid => write!(f, "\"void\""),
                other => write!(f, "\"{other:#}\""),
            }
        }
    }
}

struct Printable(Vec<Token>);

impl Display for Printable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indentation = &mut 0usize;
        let mut newline =
            |f: &mut std::fmt::Formatter<'_>, inc: bool, dec: bool| -> std::fmt::Result {
                if inc {
                    *indentation = indentation.saturating_add(1);
                }
                if dec {
                    *indentation = indentation.saturating_sub(1);
                }

                writeln!(f)?;
                if *indentation > 0 {
                    write!(f, "{}", "\t".repeat(*indentation))?;
                }

                Ok(())
            };

        for token in self.0.iter() {
            match token {
                Token::CloseBrace => {
                    newline(f, false, true)?;
                }
                _ => {}
            };

            write!(f, "{token:#}")?;

            match token {
                Token::OpenBrace => {
                    newline(f, true, false)?;
                }
                Token::Semicolon => {
                    newline(f, false, false)?;
                }
                _ => {}
            };
        }
        Ok(())
    }
}

pub fn printable(tree: impl ToTokens) -> impl Display {
    Printable(tree.to_tokens().collect())
}
