use super::lex as super_lex;
use super::*;

fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    let tokens = super_lex(source)?;
    Ok(tokens.into_iter().map(|s| s.0).collect())
}

#[test]
fn extra_paren() {
    use Token::*;
    assert_eq!(
        Ok(vec![OpenParen, Constant(3), CloseParen, CloseParen]),
        lex("(3))")
    );
}

#[test]
fn regex_special_chars() {
    use Token::*;
    assert_eq!(Ok(vec![Plus, Star, Caret, DoublePipe, Pipe]), lex("+*^|||"));
}

#[test]
fn assignment_operators() {
    use Token::*;
    assert_eq!(
        Ok(vec![
            NotEqual,
            PercentEqual,
            AmpersandEqual,
            MinusEqual,
            ForwardSlashEqual,
            LeftShiftEqual,
            RightShiftEqual,
            StarEqual,
            PlusEqual,
            CaretEqual,
            PipeEqual
        ]),
        lex("!= %= &= -= /= <<= >>= *= += ^= |=")
    );
}
