use crate::parser::Program;
use crate::parser::visit_mut::VisitMutExt;
use crate::semantic::SemanticError;
use crate::semantic::SemanticErrors;
use crate::semantic::goto::*;

use super::parse;

fn run_analysis(mut tree: Program) -> Result<(), SemanticErrors> {
    let mut visitor = collect_duplicates();
    visitor.visit_mut_program(&mut tree);
    let labels = visitor.into()?;

    let mut visitor = find_missing(labels);
    visitor.visit_mut_program(&mut tree);
    visitor.into()
}

#[test]
fn duplicate_labels() {
    let tree = parse(
        r#"
        int main(void) {
            int x = 0;
        label:
            x = 1;
        label:
            return 2;
        }
    "#,
    );
    let result = run_analysis(tree);
    let mut err = result.expect_err("expected Err, got Ok");
    assert_eq!(err.0.len(), 1);
    let err = err.0.pop().unwrap();
    let err = match err {
        SemanticError::GotoError(err) => err,
        otherwise => panic!("expected GotoError, found {otherwise}"),
    };
    assert!(
        matches!(err, Error::DuplicateLabel { .. }),
        "expected to get a duplicate label error, got {}",
        err
    );
}

#[test]
fn missing_label() {
    let tree = parse(
        r#"
        int main(void) {
            goto label;
            return 0;
        }
    "#,
    );
    let result = run_analysis(tree);
    let mut err = result.expect_err("expected Err, got Ok");
    assert_eq!(err.0.len(), 1);
    let err = err.0.pop().unwrap();
    let err = match err {
        SemanticError::GotoError(err) => err,
        otherwise => panic!("expected GotoError, found {otherwise}"),
    };
    assert!(
        matches!(err, Error::MissingLabel { .. }),
        "expected to get a duplicate label error, got {}",
        err
    );
}
