use htmlform::{ValueMap, Value, urldecode, UrlDecodingError};

#[test]
fn test_parse_urlencoded_one_key_one_val() {
    let values = ValueMap::from_urlencoded(b"foo=1").unwrap();
    assert_eq!(values.len(), 1);
    assert_eq!(
        values.get("foo").unwrap(), &vec![Value::new("1")]);
}

#[test]
fn test_parse_urlencoded_one_key_no_val() {
    let values = ValueMap::from_urlencoded(b"foo").unwrap();
    assert_eq!(values.len(), 1);
    assert_eq!(
        values.get("foo").unwrap(), &vec![Value::new("")]);
}

#[test]
fn test_parse_urlencoded_one_key_two_vals() {
    let values = ValueMap::from_urlencoded(b"foo=1&foo=2").unwrap();
    assert_eq!(values.len(), 1);
    assert_eq!(
        values.get("foo").unwrap(),
        &vec![Value::new("1"), Value::new("2")]);
}

#[test]
fn test_parse_urlencoded_two_keys() {
    let values = ValueMap::from_urlencoded(b"foo=1&bar=2").unwrap();
    assert_eq!(values.len(), 2);
    assert_eq!(
        values.get("foo").unwrap(), &vec![Value::new("1")]);
    assert_eq!(
        values.get("bar").unwrap(), &vec![Value::new("2")]);
}

#[test]
fn test_parse_urlencoded_encoded_correctly() {
    let values = ValueMap::from_urlencoded(b"foo=foo%20bar").unwrap();
    assert_eq!(values.len(), 1);
    assert_eq!(
        values.get("foo").unwrap(), &vec![Value::new("foo bar")]);
}

#[test]
fn test_parse_urlencoded_encoded_invalid_char() {
    assert_eq!(
        ValueMap::from_urlencoded(b"foo=foo%2xbar"),
        Err(UrlDecodingError::new("invalid encoding sequence")));
}

#[test]
fn test_parse_urldecode_plus() {
    assert_eq!(urldecode(b"foo+bar").unwrap(), "foo bar");
}
