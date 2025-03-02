use super::*;

use miette::Diagnostic;

fn assert_labels_eq<'a>(
    expected: &'a [(Option<String>, (usize, usize))],
    actual: Box<dyn Iterator<Item = miette::LabeledSpan> + 'a>,
) {
    let actual = actual
        .map(|span| {
            (
                span.label().map(ToString::to_string),
                (span.offset(), span.len()),
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(expected, actual);
}

#[test]
fn labels_unexpected() {
    let err = ParseError::UnexpectedToken {
        expected: Token::Plus,
        actual: Span<Token> {
            token: Token::Star,
            span: (1, 1).into(),
        },
    };
    let labels = err.labels();
    assert!(labels.is_some());
    assert_labels_eq(&[(Some("here".to_string()), (1, 1))], labels.unwrap());
}

#[test]
fn labels_context() {
    let err = ParseError::Context {
        node_name: "Function".to_string(),
        err: Box::new(ParseError::UnexpectedToken {
            expected: Token::Plus,
            actual: Span<Token> {
                token: Token::Star,
                span: (1, 1).into(),
            },
        }),
    };
    let labels = err.labels();
    assert!(labels.is_some());
    assert_labels_eq(&[(Some("here".to_string()), (1, 1))], labels.unwrap());
}
