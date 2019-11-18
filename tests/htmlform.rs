use es_htmlform::{HtmlForm, FormError, Field};
use es_htmlform::value::{ValueMap, Value};
use es_htmlform::types::{Method, Element, InputType, Attr, Constraint as C};

fn testform() -> Result<HtmlForm<'static>, FormError> {
    Ok(HtmlForm::new(".", Method::Post)
        .input(
            InputType::Text, "foo", "Foo", true,
            vec![C::MinLength(0), C::MaxLength(10)],
            vec![])?
        .input(
            InputType::Text, "bar", "Bar", true,
            vec![
                C::MinLength(0), C::MaxLength(10), C::Pattern("^[a-z]+$")],
            vec![])?
        .input(
            InputType::Number, "baz", "Baz", false,
            vec![
                C::MinNumber(0.0), C::MaxNumber(10.0),
            ],
            vec![Attr::StepFloat(0.1)])?)
}

fn testform_new_api() -> Result<HtmlForm<'static>, FormError> {
    Ok(HtmlForm::new(".", Method::Post)
        .add(
            Field::text("foo", "Foo", true)
                .constraints(
                    vec![C::MinLength(0), C::MaxLength(10)])?)
        .add(
            Field::text("bar", "Bar", true)
                .constraints(
                    vec![
                        C::MinLength(0), C::MaxLength(10),
                        C::Pattern("^[a-z]+$")])?)
        .add(
            Field::number("baz", "Baz", false)
                .constraints(vec![C::MinNumber(0.0), C::MaxNumber(10.0)])?
                .attributes(vec![Attr::StepFloat(0.1)])?)
    )
}

#[test]
fn test_form_build() {
    let form = testform_new_api().unwrap();
    assert_eq!(form.fields.len(), 3);
}

#[test]
fn test_form_validation_success() {
    let values = ValueMap::from_urlencoded(
        b"foo=1&bar=abc&baz=3").unwrap();
    let mut form = testform_new_api().unwrap();
    form.update(&values, true);
    assert_eq!(form.errors.len(), 0);
}

#[test]
fn test_form_validation_missing_required() {
    let values = ValueMap::from_urlencoded(b"foo=1&baz=3").unwrap();
    let mut form = testform_new_api().unwrap();
    form.update(&values, true);
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
    let mut form = HtmlForm::new(".", Method::Post)
        .input(
            InputType::Text, "foo", "Foo", true,
            vec![C::Func(Box::new(|_| Ok(())))], vec![],
        ).unwrap();
    form.update(&values, true);
    assert_eq!(form.errors.len(), 0);
}

#[test]
fn test_constraint_not_allowed() {
    let result = HtmlForm::new(".", Method::Post)
        .input(
            InputType::Date, "foo", "Foo", true,
            vec![C::MaxNumber(5.0)], vec![]);
    assert!(result.is_err());
}

#[test]
fn test_element_value_not_allowed() {
    let result = HtmlForm::new(".", Method::Post)
        .element(
            Element::Input(InputType::Email), "foo", "Foo",
            true, Some(vec!["noemail"]), &[], vec![], vec![]);
    assert!(result.is_err());
}

#[test]
fn modify_field() {
    let mut form = testform_new_api().unwrap();
    assert!(form.get_string("foo").is_err());
    form.fields[0].set_values(vec![&Value::new("foo")]);
    assert_eq!(form.get_string("foo").unwrap(), "foo");
}
