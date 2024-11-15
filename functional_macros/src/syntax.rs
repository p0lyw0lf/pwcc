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
use crate::types::Struct;
use crate::types::StructField;
use crate::types::StructStruct;

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

/// If all the expressions are Some(Err), returns Some(Err) with all their errors joined. If any
/// return None, cause the containing function to return None as well. Otherwise, break as soon as
/// we encounter one that returns Some(Ok).
macro_rules! any {
    ($iter:expr, $($f:ident ($($arg:expr),*)),+ $(,)?) => {
        {
        let outer_iter = $iter;
        'outer: {
            let mut err = Option::<$crate::errors::ParseErrors>::None;
            $(
                let mut inner_iter = outer_iter.clone();
                let iter = &mut inner_iter;
                match $f(iter, $($arg,)*) {
                    None => break 'outer None,
                    Some(Ok(_)) => {
                        *outer_iter = inner_iter;
                        break 'outer Some(Ok(()));
                    }
                    Some(Err(b)) => match err {
                        Some(a) => err = Some(a + b),
                        None => err = Some(b.into()),
                    }
                };
            )+
            Some(Err(err.unwrap()))
        }
        }
    }
}

/// Returns Some(()) if the ident is found and the iter has been advanced, otherwise returns None
fn keyword(iter: &mut token_stream::IntoIter, ident: &str) -> ParseResult {
    let mut p = iter.clone();
    Some(match p.next()? {
        TokenTree::Ident(i) if i.to_string() == ident => {
            *iter = p;
            Ok(())
        }
        other => Err(ParseError::ExpectedKeyword {
            expected: ident.into(),
            actual: other,
        }
        .into()),
    })
}

/// Returns Some w/ the ident and advance the iter if it's found, otherwise returns None
fn ident(iter: &mut token_stream::IntoIter) -> ParseResult<Ident> {
    let mut p = iter.clone();
    Some(match p.next()? {
        TokenTree::Ident(i) => {
            *iter = p;
            Ok(i)
        }
        other => Err(ParseError::ExpectedIdent { actual: other }.into()),
    })
}

/// Returns Some w/ the group's TokenStream and advances `iter` if the next token is a group,
/// otherwise returns None
fn group(iter: &mut token_stream::IntoIter, delimiter: Delimiter) -> ParseResult<Group> {
    let mut p = iter.clone();
    Some(match p.next()? {
        TokenTree::Group(g) if g.delimiter() == delimiter => {
            *iter = p;
            Ok(g)
        }
        other => Err(ParseError::ExpectedGroup {
            expected_delim: delimiter,
            actual: other,
        }
        .into()),
    })
}

fn punct(iter: &mut token_stream::IntoIter, punct: &[char]) -> ParseResult {
    let mut p = iter.clone();
    for ch in punct {
        match p.next()? {
            TokenTree::Punct(p) if p == *ch => {}
            other => {
                return Some(Err(ParseError::ExpectedPunct {
                    expected_chs: punct.into(),
                    actual: other,
                }
                .into()));
            }
        }
    }

    *iter = p;
    Some(Ok(()))
}

pub fn item(iter: &mut token_stream::IntoIter) -> ParseResult {
    // Chomp as many outer attributes as we can
    loop {
        let res = maybe!(outer_attribute(iter));
        if res.is_err() {
            break;
        }
    }

    // Doesn't matter if we find visibility or not
    let _ = maybe!(visibility(iter));

    any! {
        iter,
        module(),
        r#struct(),
        r#enum(),
    }
}

pub fn visibility(iter: &mut token_stream::IntoIter) -> ParseResult<TokenStream> {
    always!(keyword(iter, "pub"));
    let group = maybe!(group(iter, Delimiter::Parenthesis));
    Some(Ok(group.map(|g| g.stream()).unwrap_or(TokenStream::new())))
}

pub fn module(iter: &mut token_stream::IntoIter) -> ParseResult {
    always!(keyword(iter, "mod"));
    let _ = always!(ident(iter));
    let group = always!(group(iter, Delimiter::Brace));

    let iter = &mut group.stream().into_iter();
    loop {
        match any! {
            &mut *iter,
            inner_attribute(),
            item(),
        } {
            // Finished parsing the module, return successfully
            None => break,
            // Error parsing the module, return
            Some(Err(e)) => return Some(Err(e)),
            Some(Ok(())) => {}
        }
    }

    Some(Ok(()))
}

pub fn inner_attribute(iter: &mut token_stream::IntoIter) -> ParseResult<TokenStream> {
    always!(punct(iter, &['#', '!']));
    let group = always!(group(iter, Delimiter::Bracket));
    Some(Ok(group.stream()))
}

pub fn outer_attribute(iter: &mut token_stream::IntoIter) -> ParseResult<TokenStream> {
    always!(punct(iter, &['#']));
    let group = always!(group(iter, Delimiter::Bracket));
    Some(Ok(group.stream()))
}

pub fn r#struct(iter: &mut token_stream::IntoIter) -> ParseResult {
    always!(keyword(iter, "struct"));
    let ident = always!(ident(iter));

    let generic_params = maybe!(generic_params(iter)).ok();
    let where_clause = maybe!(where_clause(iter)).ok();

    let base = &Struct {
        ident,
        generic_params,
        where_clause,
    };

    any! {
        iter,
        struct_struct(base.clone()),
        struct_singleton(base.clone()),
        tuple_struct(base.clone()),
    }
}

fn struct_struct(iter: &mut token_stream::IntoIter, base: Struct) -> ParseResult {
    let group = always!(group(iter, Delimiter::Brace));

    let iter = &mut group.stream().into_iter();
    let mut fields = Vec::<StructField>::new();

    loop {
        match struct_field(iter) {
            None => break,
            Some(Err(e)) => return Some(Err(e)),
            Some(Ok(f)) => fields.push(f),
        }
        let _ = maybe!(punct(iter, &[',']));
    }

    let s = StructStruct { base, fields };

    println!("parsed {s:?}");

    Some(Ok(()))
}

fn struct_field(iter: &mut token_stream::IntoIter) -> ParseResult<StructField> {
    loop {
        let res = maybe!(outer_attribute(iter));
        if res.is_err() {
            break;
        }
    }
    let _ = maybe!(visibility(iter));

    let field_ident = always!(ident(iter));
    always!(punct(iter, &[':']));

    // NOTE: we're simplifying the type parsing for convenience.
    let r#type = always!(ident(iter));
    let generic_params = maybe!(generic_params(iter)).ok();

    Some(Ok(StructField {
        ident: field_ident,
        r#type,
        generic_params,
    }))
}

fn struct_singleton(iter: &mut token_stream::IntoIter, base: Struct) -> ParseResult {
    always!(punct(iter, &[';']));

    println!("parsed {base:?}");

    Some(Ok(()))
}

fn tuple_struct(iter: &mut token_stream::IntoIter, base: Struct) -> ParseResult {
    let group = always!(group(iter, Delimiter::Parenthesis));
    always!(punct(iter, &[';']));

    println!("parsed tuple struct: {group}");

    Some(Ok(()))
}

pub fn generic_params(iter: &mut token_stream::IntoIter) -> ParseResult<TokenStream> {
    // TODO: may need to make some version of this that 
    always!(punct(iter, &['<']));

    let mut out = TokenStream::new();

    let mut level: usize = 0;
    loop {
        let tok = iter.next()?;
        match tok.clone() {
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
        out.extend([tok]);
    }

    Some(Ok(out))
}

pub fn where_clause(_iter: &mut token_stream::IntoIter) -> ParseResult<TokenStream> {
    Some(Err(ParseError::NotImplemented.into()))
}

pub fn r#enum(iter: &mut token_stream::IntoIter) -> ParseResult {
    always!(keyword(iter, "enum"));
    let ident = always!(ident(iter));

    let _ = maybe!(generic_params(iter));
    let _ = maybe!(where_clause(iter));

    let cases = always!(group(iter, Delimiter::Brace));

    println!("parsed enum {ident}: {cases}");

    Some(Ok(()))
}
