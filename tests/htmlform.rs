use htmlform::{HtmlForm, ValueMap};
use htmlform::types::{Method, InputType, Attr, Constraint};

fn testform() -> HtmlForm<'static> {
    HtmlForm::new(".", Method::Post)
        .input(
            InputType::Text, "foo", "Foo", true, None,
            vec![Constraint::MinLength(0), Constraint::MaxLength(10)],
            vec![]).unwrap()
        .input(
            InputType::Text, "bar", "Bar", true, None,
            vec![
                Constraint::MinLength(0),
                Constraint::MaxLength(10),
                Constraint::Pattern("^[a-z]+$")],
            vec![]).unwrap()
        .input(
            InputType::Number, "baz", "Baz", false, None,
            vec![
                Constraint::MinNumber(0.0),
                Constraint::MaxNumber(10.0),
            ],
            vec![Attr::StepFloat(0.1)]).unwrap()
}

#[test]
fn test_form_build() {
    let form = testform();
    assert_eq!(form.fields.len(), 3);
}

#[test]
fn test_form_validation_success() {
    let values = ValueMap::from_urlencoded(
        b"foo=1&bar=abc&baz=3").unwrap();
    let form = testform()
        .validate_and_set(values);
    assert_eq!(form.errors.len(), 0);
}

#[test]
fn test_form_validation_missing_required() {
    let values = ValueMap::from_urlencoded(b"foo=1&baz=3").unwrap();
    let form = testform()
        .validate_and_set(values);
    assert_eq!(form.errors.len(), 1);
    assert_eq!(
        form.errors.keys().collect::<Vec<&String>>(), vec!["bar"]);
    assert_eq!(
        form.errors.values().collect::<Vec<&String>>(),
        vec!["no value for required field"]);
}

#[test]
fn test_form_validation_func() {
    let values = ValueMap::from_urlencoded(b"foo=1").unwrap();
    let form = HtmlForm::new(".", Method::Post)
        .input(
            InputType::Text, "foo", "Foo", true, None,
            vec![Constraint::Func(Box::new(|_| Ok(())))], vec![],
        ).unwrap()
        .validate_and_set(values);
    assert_eq!(form.errors.len(), 0);
}

#[test]
fn test_constraint_not_allowed() {
    let result = HtmlForm::new(".", Method::Post)
        .input(
            InputType::Color, "foo", "Foo", true, None,
            vec![Constraint::MaxNumber(5.0)], vec![]);
    assert!(result.is_err());
}

#[test]
fn test_element_value_not_allowed() {
    let result = HtmlForm::new(".", Method::Post)
        .input(
            InputType::Email, "foo", "Foo", true, Some("noemail"),
            vec![], vec![]);
    assert!(result.is_err());
}
