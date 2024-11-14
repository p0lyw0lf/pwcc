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
}

impl Display for ParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use ParseError::*;
        match self {
            ExpectedKeyword { expected, actual } => {
                write!(
                    f,
                    "expected ident \"{}\", got {}",
                    expected,
                    actual,
                )
            }
            ExpectedIdent { actual } => {
                write!(f, "expected ident, got {}", actual)
            }
            ExpectedGroup {
                expected_delim,
                actual,
            } => {
                write!(
                    f,
                    "expected group with delimiter \"{:?}\", got {}",
                    expected_delim,
                    actual,
                )
            }
            ExpectedPunct {
                expected_chs,
                actual,
            } => {
                write!(
                    f,
                    "expected punctuation '{}', got {}",
                    expected_chs
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(""),
                    actual,
                )
            }
        }
    }
}

impl Error for ParseError {}

#[derive(Debug)]
pub struct ParseErrors(Vec<ParseError>);

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

/// Ok(Some(t)) == parse was successful
/// Ok(None)    == end of stream found
/// Err(e)      == encountered parse error
pub type ParseResult<T> = Result<Option<T>, ParseErrors>;
