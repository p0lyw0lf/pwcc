use core::error::Error;
use core::fmt::Display;
use proc_macro::Delimiter;
use proc_macro::TokenTree;

#[derive(Debug)]
pub enum ParseError {
    ExpectedKeyword {
        expected: String,
        actual: TokenTree,
    },
    ExpectedIdent {
        actual: TokenTree,
    },
    ExpectedGroup {
        expected_delim: Delimiter,
        actual: TokenTree,
    },
    ExpectedPunct {
        expected_chs: Vec<char>,
        actual: TokenTree,
    },
    Context(String, ParseErrors),
    NotImplemented,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use ParseError::*;
        let indent_level = f.width().unwrap_or(0);
        let indent_str = core::iter::repeat(" ")
            .take(indent_level)
            .collect::<String>();
        match self {
            ExpectedKeyword { expected, actual } => {
                write!(
                    f,
                    "{indent_str}expected ident \"{}\", got {}",
                    expected, actual,
                )
            }
            ExpectedIdent { actual } => {
                write!(f, "{indent_str}expected ident, got {}", actual)
            }
            ExpectedGroup {
                expected_delim,
                actual,
            } => {
                write!(
                    f,
                    "{indent_str}expected group with delimiter \"{:?}\", got {}",
                    expected_delim, actual,
                )
            }
            ExpectedPunct {
                expected_chs,
                actual,
            } => {
                write!(
                    f,
                    "{indent_str}expected punctuation '{}', got {}",
                    expected_chs
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(""),
                    actual,
                )
            }
            Context(ctx, errs) => {
                write!(f, "{indent_str}{ctx}: {errs}")
            }
            NotImplemented => write!(f, "not implemented"),
        }
    }
}

impl Error for ParseError {}

#[derive(Debug)]
pub struct ParseErrors(Vec<ParseError>);

impl ParseErrors {
    pub fn context(self, context: impl ToString) -> ParseErrors {
        ParseError::Context(context.to_string(), self).into()
    }
}

impl From<ParseError> for ParseErrors {
    fn from(value: ParseError) -> Self {
        Self([value].into())
    }
}

impl Display for ParseErrors {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for err in self.0.iter() {
            writeln!(f, "{}", err)?;
        }
        Ok(())
    }
}

// Don't have Semigroup here, so making do with operator overloading :P
impl core::ops::Add<ParseError> for ParseErrors {
    type Output = ParseErrors;
    fn add(mut self, rhs: ParseError) -> Self::Output {
        self.0.push(rhs);
        self
    }
}

impl core::ops::Add<ParseErrors> for ParseErrors {
    type Output = ParseErrors;
    fn add(mut self, rhs: ParseErrors) -> Self::Output {
        self.0.extend(rhs.0.into_iter());
        self
    }
}

impl core::ops::Add<ParseError> for ParseError {
    type Output = ParseErrors;
    fn add(self, rhs: ParseError) -> Self::Output {
        ParseErrors([self, rhs].into())
    }
}

impl core::ops::Add<ParseErrors> for ParseError {
    type Output = ParseErrors;
    fn add(self, rhs: ParseErrors) -> Self::Output {
        let mut errs = Vec::from([self]);
        errs.extend(rhs.0);
        ParseErrors(errs)
    }
}

impl Error for ParseErrors {}

/// Some(Ok((t)) == parse was successful
/// Some(Err(e)) == encountered parse error
/// None         == end of stream found
pub type ParseResult<T = ()> = Option<Result<T, ParseErrors>>;
