//! A collection of functions that test for parts of the Rust grammar, as defined by The Rust
//! Reference. We extra data only as necessary for this crate.

use proc_macro::token_stream;
use proc_macro::Delimiter;
use proc_macro::Group;
use proc_macro::Ident;
use proc_macro::TokenStream;
use proc_macro::TokenTree;

use crate::errors::ParseError;
use crate::errors::ParseResult;

/// Turns a ParseResult<T> into a Result<T>, early-returning if it's an Ok(None)
macro_rules! expect {
    ($e:expr) => {
        {
            let e = $e;
            match e {
                Ok(Some(t)) => Ok(t),
                Ok(None) => {
                    return Ok(None);
                }
                Err(e) => Err(e),
            }
        }
    }
}

/// Returns Some(()) if the ident is found and the iter has been advanced, otherwise returns None
fn parse_keyword(iter: &mut token_stream::IntoIter, ident: &str) -> ParseResult<()> {
    let mut p = iter.clone();
    match p.next() {
        Some(TokenTree::Ident(i)) if i.to_string() == ident => {
            *iter = p;
            Ok(Some(()))
        }
        Some(other) => Err(ParseError::ExpectedKeyword {
            expected: ident.into(),
            actual: other,
        }.into()),
        None => Ok(None),
    }
}

/// Returns Some w/ the ident and advance the iter if it's found, otherwise returns None
fn parse_ident(iter: &mut token_stream::IntoIter) -> ParseResult<Ident> {
    let mut p = iter.clone();
    match p.next() {
        Some(TokenTree::Ident(i)) => {
            *iter = p;
            Ok(Some(i))
        }
        Some(other) => Err(ParseError::ExpectedIdent { actual: other }.into()),
        None => Ok(None),
    }
}

/// Returns Some w/ the group's TokenStream and advances `iter` if the next token is a group,
/// otherwise returns None
fn parse_group(
    iter: &mut token_stream::IntoIter,
    delimiter: Delimiter,
) -> ParseResult<Group> {
    let mut p = iter.clone();
    match p.next() {
        Some(TokenTree::Group(g)) if g.delimiter() == delimiter => {
            *iter = p;
            Ok(Some(g))
        }
        Some(other) => Err(ParseError::ExpectedGroup {
            expected_delim: delimiter,
            actual: other,
        }.into()),
        None => Ok(None),
    }
}

fn parse_punct(iter: &mut token_stream::IntoIter, punct: &[char]) -> ParseResult<()> {
    let mut p = iter.clone();
    for ch in punct {
        match p.next() {
            Some(TokenTree::Punct(p)) if p == *ch => {}
            Some(other) => {
                return Err(ParseError::ExpectedPunct {
                    expected_chs: punct.into(),
                    actual: other,
                }.into());
            },
            None => {
                return Ok(None);
            },
        }
    }

    *iter = p;
    Ok(Some(()))
}

pub fn parse_item(iter: &mut token_stream::IntoIter) -> ParseResult<()> {
    // Chomp as many outer attributes as we can
    loop {
        let res = expect!(parse_outer_attribute(iter));
        if res.is_err() {
            break;
        }
    }

    // Doesn't matter if we find visibility or not
    let _ = expect!(parse_visibility(iter));

    let module = expect!(parse_module(iter));
    let s = expect!(parse_struct(iter));
    let e = expect!(parse_enum(iter));


    Ok(Some(()))
}

pub fn parse_visibility(iter: &mut token_stream::IntoIter) -> ParseResult<TokenStream> {
    expect!(parse_keyword(iter, "pub"))?;
    let group = parse_group(iter, Delimiter::Parenthesis)?;
    Ok(Some(group
        .map(|g| g.stream())
        .unwrap_or(TokenStream::new())))
}

pub fn parse_module(iter: &mut token_stream::IntoIter) -> ParseResult<()> {
    expect!(parse_keyword(iter, "mod"))?;
    let _ = expect!(parse_ident(iter))?;
    let group = expect!(parse_group(iter, Delimiter::Brace))?;

    let iter = &mut group.stream().into_iter();
    loop {
        let inner_attr = parse_inner_attribute(iter);
        let item = parse_item(iter);

        let inner_attr_err = match inner_attr {
            Ok(Some(_inner_attr)) => {
                println!("parsed inner attribute");
                None
            },
            Ok(None) => {
                // Parsing complete
                break;
            }
            Err(a) => Some(a),
        };

        match item {
            Ok(Some(_item)) => {
                println!("parsed item");
            },
            Ok(None) => {
                // Parsing complete
                break;
            }
            Err(b) => {
                if let Some(a) = inner_attr_err {
                    return Err(a + b);
                }
            }
        };
    }

    Ok(Some(()))
}

pub fn parse_inner_attribute(iter: &mut token_stream::IntoIter) -> ParseResult<TokenStream> {
    expect!(parse_punct(
        iter,
        &[
            '#',
            '!',
        ],
    ))?;
    let group = expect!(parse_group(iter, Delimiter::Bracket))?;
    Ok(Some(group.stream()))
}

pub fn parse_outer_attribute(iter: &mut token_stream::IntoIter) -> ParseResult<TokenStream> {
    expect!(parse_punct(
        iter,
        &[
            '#',
        ],
    ))?;
    let group = expect!(parse_group(iter, Delimiter::Bracket))?;
    Ok(Some(group.stream()))
}

pub fn parse_struct(iter: &mut token_stream::IntoIter) -> ParseResult<()> {
    expect!(parse_keyword(iter, "struct"))?;
    let ident = expect!(parse_ident(iter))?;

    let _ = expect!(parse_generic_params(iter));
    let _ = expect!(parse_where_clause(iter));

    let named_group = expect!(parse_group(iter, Delimiter::Brace));
    let singleton = expect!(parse_punct(iter, &[';']));
    let tuple_group = expect!(parse_group(iter, Delimiter::Parenthesis));

    if named_group.is_ok() {
        println!("parsed named struct {ident}");
    }

    if singleton.is_ok() {
        println!("parsed singleton struct {ident}");
    }

    if tuple_group.is_ok() {
        expect!(parse_punct(iter, &[';']))?;
        println!("parsed tuple struct {ident}");
    }

    Ok(Some(()))
}

pub fn parse_generic_params(iter: &mut token_stream::IntoIter) -> ParseResult<()> {
    let _ = expect!(parse_punct(iter, &['<']))?;

    let mut level: usize = 0;
    loop {
        let tok = match iter.next() {
            Some(t) => t,
            None => return Ok(None),
        };
        match tok {
            TokenTree::Punct(p) if p.as_char() == '<' => {
                level += 1;
            }
            TokenTree::Punct(p) if p.as_char() == '>' => {
                if level == 0 {
                    break;
                }
                level -= 1;
            }
            _ => {}
        }
    }

    Ok(Some(()))
}

pub fn parse_where_clause(_iter: &mut token_stream::IntoIter) -> ParseResult<()> {
    // TODO: implement
    Ok(Some(()))
}

pub fn parse_enum(iter: &mut token_stream::IntoIter) -> ParseResult<()> {
    expect!(parse_keyword(iter, "enum"))?;
    let ident = expect!(parse_ident(iter))?;

    let _ = expect!(parse_generic_params(iter));
    let _ = expect!(parse_where_clause(iter));

    let _cases = expect!(parse_group(iter, Delimiter::Brace))?;

    println!("parsed enum {ident}");

    Ok(Some(()))
}
