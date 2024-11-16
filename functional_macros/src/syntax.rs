//! Hand-rolled parser, for partial parsing of TokenTrees without the need for all of syn

use proc_macro::token_stream;
use proc_macro::Delimiter;
use proc_macro::Spacing;
use proc_macro::TokenTree;
use proc_macro::TokenStream;
use proc_macro::Ident;
use proc_macro::Group;
use proc_macro::Punct;

use core::error::Error;
use core::fmt::Display;

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
                    expected, actual,
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
                    expected_delim, actual,
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

/// Some(Ok((t)) == parse was successful
/// Some(Err(e)) == encountered parse error
/// None         == end of stream found
pub type ParseResult<T = ()> = Option<Result<T, ParseError>>;

/// Turns a ParseResult<T> into a T, early-returning if it's a None or Some(Err)
macro_rules! always {
    ($e:expr) => {{
        match $e {
            Some(Ok(t)) => t,
            Some(Err(e)) => return Some(Err(e)),
            None => return None,
        }
    }};
}

/// Turns a ParseResult<T> into a Result<T, ParseError>, early-returning if its None
macro_rules! maybe {
    ($e:expr) => {
        match $e {
            Some(v) => v,
            None => return None,
        }
    };
}

/// Returns if the ident is found and the iter has been advanced
pub fn keyword(out: &mut TokenStream, iter: &mut token_stream::IntoIter, ident: &str) -> ParseResult {
    let mut p = iter.clone();
    let i = match p.next()? {
        TokenTree::Ident(i) if i.to_string() == ident => i, 
        other => return Some(Err(ParseError::ExpectedKeyword {
            expected: ident.into(),
            actual: other,
        })),
    };

    *iter = p;
    out.extend([TokenTree::from(i)]);
    Some(Ok(()))
}

/// Returns the ident with the iter advanced if there is one
pub fn ident(out: &mut TokenStream, iter: &mut token_stream::IntoIter) -> ParseResult<Ident> {
    let mut p = iter.clone();
    let i = match p.next()? {
        TokenTree::Ident(i) => i, 
        other => return Some(Err(ParseError::ExpectedIdent { actual: other }.into())),
    };

    *iter = p;
    out.extend([TokenTree::from(i.clone())]);
    Some(Ok(i))
}

/// Returns the group's TokenStream and advances `iter` if the next token is a group. Notably, does
/// _not_ automatically push to out when it matches.
pub fn group(iter: &mut token_stream::IntoIter, delimiter: Delimiter) -> ParseResult<Group> {
    let mut p = iter.clone();
    Some(match p.next()? {
        TokenTree::Group(g) if g.delimiter() == delimiter => {
            *iter = p;
            Ok(g)
        }
        other => Err(ParseError::ExpectedGroup {
            expected_delim: delimiter,
            actual: other,
        }),
    })
}

/// Returns if the next token is the specified sequence of punctuation, otherwise 
fn punct(out: &mut TokenStream, iter: &mut token_stream::IntoIter, punct: &[char]) -> ParseResult {
    let mut p = iter.clone();
    for (i, ch) in punct.iter().enumerate() {
        match p.next()? {
            TokenTree::Punct(p) if p == *ch && 
            // Only when we're at the end, should we have Spacing::Alone
                ((p.spacing() == Spacing::Alone) == (i == punct.len())) => {}
            other => {
                return Some(Err(ParseError::ExpectedPunct {
                    expected_chs: punct.into(),
                    actual: other,
                }));
            }
        }
    }

    *iter = p;
    out.extend(punct.iter().enumerate().map(|(i, ch)| TokenTree::Punct(Punct::new(*ch, if i == punct.len() { Spacing::Alone } else { Spacing::Joint }))));
    Some(Ok(()))
}


pub fn outer_attribute(out: &mut TokenStream, iter: &mut token_stream::IntoIter) -> ParseResult {
    always!(punct(out, iter, &['#']));
    let group = always!(group(iter, Delimiter::Bracket));
    out.extend([TokenTree::from(group)]);
    Some(Ok(()))
}

pub fn visibility(out: &mut TokenStream, iter: &mut token_stream::IntoIter) -> ParseResult {
    always!(keyword(out, iter, "pub"));
    let group = maybe!(group(iter, Delimiter::Parenthesis));
    if let Ok(group) = group {
        out.extend([TokenTree::from(group)]);
    }
    Some(Ok(()))
}
