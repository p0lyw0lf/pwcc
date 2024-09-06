use std::fmt::Display;

use crate::lexer::Token;
use crate::parser::ToTokens;

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
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
    }
}

pub struct TokenStream<'a, T: IntoIterator<Item = Token> + Clone>(&'a T);

impl<'a, I: IntoIterator<Item = Token> + Clone> Display for TokenStream<'a, I> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use Token::*;

        let indentation = &mut 0usize;
        let mut newline =
            |f: &mut core::fmt::Formatter<'_>, inc: bool, dec: bool| -> core::fmt::Result {
                if inc {
                    *indentation = indentation.saturating_add(1);
                }
                if dec {
                    *indentation = indentation.saturating_sub(1);
                }

                f.write_str("\n")?;
                if *indentation > 0 {
                    f.write_str(&"\t".repeat(*indentation))?;
                }
                Ok(())
            };

        for token in self.0.clone() {
            match token {
                CloseBrace => {
                    newline(f, false, true)?;
                }
                _ => {}
            };

            write!(f, "{token}")?;

            match token {
                OpenBrace => {
                    newline(f, true, false)?;
                }
                _ => {}
            };
        }

        Ok(())
    }
}

pub fn pretty_print(tree: impl ToTokens) {
    let mut tokens = Vec::<Token>::new();
    tree.to_tokens(&mut |t| tokens.push(t));
    println!("{}", TokenStream(&mut tokens.into_iter()));
}
