use std::fmt;
use std::str::FromStr;
use std::error::Error;
use std::ops::Deref;
use std::collections::HashMap;

use serde::ser::{Serialize, Serializer, SerializeStruct};
use regex::Regex;

#[derive(Debug, PartialEq)]
pub struct UrlDecodingError {
    message: String,
}

impl UrlDecodingError {
    fn new(message: &str) -> UrlDecodingError {
        UrlDecodingError {
            message: String::from(message),
        }
    }
}

impl fmt::Display for UrlDecodingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for UrlDecodingError {
    fn description(&self) -> &str {
        &self.message
    }
}

impl From<std::str::Utf8Error> for UrlDecodingError {
    fn from(_e: std::str::Utf8Error) -> UrlDecodingError {
        UrlDecodingError {
            message: "invalid encoding error sequence".to_string(),
        }
    }
}

impl From<hex::FromHexError> for UrlDecodingError {
    fn from(_e: hex::FromHexError) -> UrlDecodingError {
        UrlDecodingError {
            message: "invalid encoding sequence".to_string(),
        }
    }
}

impl From<std::num::ParseIntError> for UrlDecodingError {
    fn from(_e: std::num::ParseIntError) -> UrlDecodingError {
        UrlDecodingError {
            message: "invalid encoding character".to_string(),
        }
    }
}


pub fn urldecode(input: &[u8]) -> Result<String, UrlDecodingError> {
    let percent: u8 = 37;
    let mut out: Vec<u8> = Vec::new();
    let mut i = 0;
    while i < input.len() {
        let chr: u8 = input[i];
        let charcode: u8;
        i += 1;
        if chr == percent {
            if input.len() < i + 2 {
                return Err(
                    UrlDecodingError::new("missing encoding character"));
            }
            // we now have 2 ascii chars (u8), which should be numbers,
            // depicting 2 character a hexdecimal number when combined, which
            // form the ascii value of 1 char (so another u8, unicode is
            // encoded as multiple u8s in a row, each with a separate
            // %, so if we handle the chars 1 at a time, we should end up
            // with a valid utf-8 sequence, assuming the character encoding
            // is utf-8 (XXX and what if it isn't?))
            charcode = hex::decode(
                &format!("{}{}", input[i] as char, input[i + 1] as char))?[0];
            i += 2;
        } else {
            charcode = chr;
        }
        out.push(charcode);
    }
    Ok(std::str::from_utf8(&out)?.to_string())
}

pub enum ContentType {
    Urlencoded,
    // XXX not yet...
    // Multipart,
    Json,
}

pub enum InputType {
    Text,
    Password,
    Radio,
    Checkbox,
    Number,
    Range,
    Date,
    DateTime,
    Month,
    Week,
    Time,
    Url,
    Email,
    Tel,
    Color,
    File,
    Search,
    Hidden,
}

pub enum Select {
    Single,
    Multi,
}

pub enum Element {
    Input(InputType),
    Textarea,
    Select(Select),
}

impl Element {
    fn fieldtype(&self) -> String {
        String::from(match &self {
            Element::Input(_) => "input",
            Element::Textarea => "textarea",
            Element::Select(_) => "select",
        })
    }

    fn subtype(&self) -> String {
        String::from(match &self {
            Element::Input(input_type) => match input_type {
                InputType::Text => "text",
                InputType::Password => "password",
                InputType::Radio => "radio",
                InputType::Checkbox => "checkbox",
                InputType::Number => "number",
                InputType::Range => "range",
                InputType::Date => "date",
                InputType::DateTime => "datetime-local",
                InputType::Month => "month",
                InputType::Week => "week",
                InputType::Time => "time",
                InputType::Url => "url",
                InputType::Email => "email",
                InputType::Tel => "tel",
                InputType::Color => "color",
                InputType::File => "file",
                InputType::Search => "search",
                InputType::Hidden => "hidden",
            },
            _ => "",
        })
    }

    fn multi(&self) -> bool {
        match self {
            Element::Input(InputType::Checkbox) =>
                true,
            Element::Select(Select::Multi) => true,
            _ => false,
        }
    }
}

pub enum Constraint {
    MinLength(usize),
    MaxLength(usize),
    MinNumber(f64),
    MaxNumber(f64),
    Pattern(String),
}

impl Constraint {
    pub fn validate(&self, formvalue: &Value)
            -> Result<(), ValidationError> {
        match self {
            Constraint::MinLength(min) => {
                let value: String = formvalue.into()?;
                if value.len() < *min {
                    return Err(ValidationError::new("value too short"));
                }
            },
            Constraint::MaxLength(max) => {
                let value: String = formvalue.into()?;
                if value.len() > *max {
                    return Err(ValidationError::new("value too short"));
                }
            },
            Constraint::MinNumber(min) => {
                let value: f64 = formvalue.into()?;
                if value < *min {
                    return Err(ValidationError::new("value too small"));
                }
            },
            Constraint::MaxNumber(max) => {
                let value: f64 = formvalue.into()?;
                if value > *max {
                    return Err(ValidationError::new("value too large"));
                }
            },
            Constraint::Pattern(pattern) => {
                let value: String = formvalue.into()?;
                let reg = match Regex::new(pattern) {
                    Ok(reg) => reg,
                    Err(_) => return Err(
                        ValidationError::new("invalid pattern")),
                };
                if !reg.is_match(&value) {
                    return Err(ValidationError::new("pattern did not match"));
                }
            },
        }
        Ok(())
    }
}

impl Serialize for Constraint {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer {
        let (attr, value) = match self {
            Constraint::MinLength(value) =>
                ("minlength", format!("{}", value)),
            Constraint::MaxLength(value) =>
                ("maxlength", format!("{}", value)),
            Constraint::MinNumber(value) =>
                ("min", format!("{}", value)),
            Constraint::MaxNumber(value) =>
                ("max", format!("{}", value)),
            Constraint::Pattern(pattern) => ("pattern", pattern.clone()),
        };
        let mut s = serializer.serialize_struct("Constraint", 2)?;
        s.serialize_field("attr", &attr)?;
        s.serialize_field("value", &value)?;
        s.end()
    }
}

pub enum Attr {
    Step(f64),
}

impl Serialize for Attr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer {
        let (attr, value) = match self {
            Attr::Step(value) =>
                ("step", format!("{}", value)),
        };
        let mut s = serializer.serialize_struct("Attr", 2)?;
        s.serialize_field("attr", &attr)?;
        s.serialize_field("value", &value)?;
        s.end()
    }
}

// XXX make translatable later
pub struct Label(String);

impl Label {
    fn new(label: &str) -> Label {
        Label(String::from(label))
    }
}

impl Deref for Label {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Serialize for Label {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer {
        serializer.serialize_str(&self.0)
    }
}

pub struct HtmlForm {
    errors: HashMap<String, String>,
    fields: Vec<Field>,
}

impl HtmlForm {
    pub fn new() -> HtmlForm {
        HtmlForm {
            errors: HashMap::new(),
            fields: Vec::new(),
        }
    }

    pub fn from_request(_content_type: &ContentType, _payload: String)
            -> HtmlForm {
        HtmlForm {
            errors: HashMap::new(),
            fields: Vec::new(),
        }
    }

    pub fn validate_and_set(
            mut self, values: &ValueMap)
            -> Self {
        for field in &mut self.fields {
            let non_empty = values.non_empty_values(&field.name);
            if non_empty.len() > 0 {
                if !field.multi && non_empty.len() > 1 {
                    self.errors.insert(
                        field.name.clone(),
                        String::from("field can only have one value"));
                } else {
                    match field.validate_and_set(&non_empty) {
                        Err(e) => {
                            self.errors.insert(
                                field.name.clone(), format!("{}", e));
                        },
                        Ok(()) => (),
                    }
                }
            } else {
                // set default (empty) value
                field.set_default_value();
                match field.required {
                    true => {
                        self.errors.insert(
                            field.name.clone(),
                            String::from("no value for required field"));
                    },
                    false => (),
                }
            }
        }
        self
    }

    pub fn textinput(
            mut self, name: &str, label: &str, required: bool,
            minlength: usize, maxlength: Option<usize>,
            pattern: Option<&str>)
            -> Self {
        let mut constraints = vec![Constraint::MinLength(minlength)];
        match maxlength {
            Some(maxlength) => {
                constraints.push(Constraint::MaxLength(maxlength));
            },
            None => (),
        }
        match pattern {
            Some(pattern) => {
                constraints.push(
                    Constraint::Pattern(String::from(pattern)));
            },
            None => (),
        }
        self.fields.push(Field::new(
            name, Label::new(label), Element::Input(InputType::Text),
            required, None, constraints, vec![]));
        self
    }

    pub fn numberinput(
            mut self, name: &str, label: &str, required: bool,
            min: Option<f64>, max: Option<f64>, step: Option<f64>)
            -> Self {
        let mut constraints = vec![];
        match min {
            Some(min) => {
                constraints.push(Constraint::MinNumber(min));
            },
            None => (),
        }
        match max {
            Some(max) => {
                constraints.push(Constraint::MaxNumber(max));
            },
            None => (),
        }
        let mut attributes = vec![];
        match step {
            Some(step) => {
                attributes.push(Attr::Step(step));
            },
            None => (),
        }
        self.fields.push(Field::new(
            name, Label::new(label), Element::Input(InputType::Number),
            required, None, constraints, attributes));
        self
    }

    pub fn textarea(
            mut self, name: &str, label: &str, required: bool)
            -> Self {
        self.fields.push(Field::new(
            name, Label::new(label), Element::Textarea,
            required, None, vec![], vec![]));
        self
    }
}

impl Serialize for HtmlForm {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer {
        let mut s = serializer.serialize_struct("Field", 2)?;
        s.serialize_field("errors", &self.errors)?;
        s.serialize_field("fields", &self.fields)?;
        s.end()
    }
}

pub struct Field {
    name: String,
    label: Label,
    fieldtype: String,
    subtype: String,
    required: bool,
    multi: bool,
    choices: Vec<(String, Label)>,
    value: Option<Value>,
    multi_value: Option<Vec<Value>>,
    attributes: Vec<Attr>,
    constraints: Vec<Constraint>,
}

impl Field {
    fn new(name: &str, label: Label, element: Element, required: bool,
            choices: Option<Vec<(String, Label)>>,
            constraints: Vec<Constraint>, attributes: Vec<Attr>)
            -> Field {
        Field {
            name: String::from(name),
            label: label,
            fieldtype: element.fieldtype(),
            subtype: element.subtype(),
            required: required,
            multi: element.multi(),
            choices: match choices {
                Some(choices) => choices,
                None => Vec::new(),
            },
            constraints: constraints,
            attributes: attributes,
            value: None,
            multi_value: None,
        }
    }

    fn validate_and_set(&self, values: &Vec<&Value>)
            -> Result<(), ValidationError> {
        if self.required && values.len() > 0 &&
                values[0].into::<String>()? == String::from("") {
            for constraint in self.constraints.iter() {
                for value in values.iter() {
                    constraint.validate(&value)?;
                }
            }
        }
        Ok(())
    }

    fn set_default_value(&mut self) {
        match self.multi {
            true => self.multi_value = Some(Vec::new()),
            false => self.value = Some(Value {value: String::from("")}),
        };
    }
}

impl Serialize for Field {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer {
        let mut s = serializer.serialize_struct("Field", 9)?;
        s.serialize_field("name", &self.name)?;
        s.serialize_field("label", &self.label)?;
        s.serialize_field("fieldtype", &self.fieldtype)?;
        s.serialize_field("subtype", &self.subtype)?;
        s.serialize_field("required", &self.required)?;
        s.serialize_field("multi", &self.multi)?;
        s.serialize_field("choices", &self.choices)?;
        s.serialize_field("constraints", &self.constraints)?;
        s.serialize_field("attributes", &self.attributes)?;
        match self.multi {
            true => {
                s.serialize_field("value", &self.multi_value)?;
            },
            false => {
                s.serialize_field("value", &self.value)?;
            },
        }
        s.end()
    }
}

#[derive(Debug, PartialEq)]
pub struct ValueMap {
    values: HashMap<String, Vec<Value>>,
}

impl ValueMap {
    fn non_empty_values(&self, key: &str) -> Vec<&Value> {
        let values = match self.values.get(key) {
            Some(value) => value,
            None => return Vec::new(),
        };
        values
            .iter()
            .filter(|v| {
                v.value != ""
            })
            .collect()
    }

    pub fn from_urlencoded(input: &[u8]) ->
            Result<ValueMap, UrlDecodingError> {
        let mut values: HashMap<String, Vec<Value>> = HashMap::new();
        let eq: u8 = 61;
        let amp: u8 = 38;
        let mut bkey = vec![];
        let mut bvalue = vec![];
        let mut in_value = false;
        for ref chr in input {
            let chr = *chr.clone();
            if chr == eq {
                if !in_value {
                    in_value = true;
                } else {
                    return Err(
                        UrlDecodingError::new("unexpected = character"));
                }
            } else if chr == amp {
                let key = urldecode(&bkey)?;
                let value = urldecode(&bvalue)?;
                if bvalue.len() > 0 && values.contains_key(&key) {
                    let keyvalues = values.get_mut(&key).unwrap();
                    keyvalues.push(Value::new(&value));
                } else {
                    values.insert(key, vec![Value::new(&value)]);
                }
                bkey.truncate(0);
                bvalue.truncate(0);
                in_value = false;
            } else if in_value {
                bvalue.push(chr);
            } else {
                bkey.push(chr);
            }
        }
        // there should now be 1 key (or pair) left in the buffers
        if bkey.len() > 0{
            let key = urldecode(&bkey)?;
            let value = urldecode(&bvalue)?;
            if bvalue.len() > 0 && values.contains_key(&key) {
                let keyvalues = values.get_mut(&key).unwrap();
                keyvalues.push(Value::new(&value));
            } else {
                values.insert(key, vec![Value::new(&value)]);
            }
        }
        Ok(ValueMap {values: values})
    }
}

impl Deref for ValueMap {
    type Target = HashMap<String, Vec<Value>>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

#[derive(Debug, PartialEq)]
pub struct Value {
    value: String,
}

impl Value {
    fn new(value: &str) -> Value {
        Value {
            value: String::from(value),
        }
    }

    fn into<T>(&self) -> Result<T, ValidationError>
            where T: FromStr {
        match self.value.parse() {
            Ok(value) => Ok(value),
            Err(_) => Err(ValidationError::new("invalid value")),
        }
    }
}

// Deref
impl Deref for Value {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer {
        serializer.serialize_str(&self.value)
    }
}

#[derive(Debug)]
pub struct ValidationError {
    message: String,
}

impl ValidationError {
    fn new(message: &str) -> ValidationError {
        ValidationError {
            message: String::from(message),
        }
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for ValidationError {
    fn description(&self) -> &str {
        &self.message
    }
}

#[cfg(test)]
mod tests {
    use crate::{HtmlForm, Value, ValueMap, UrlDecodingError};

    fn testform() -> HtmlForm {
        HtmlForm::new()
            .textinput("foo", "Foo", true, 0, Some(10), None)
            .textinput("bar", "Bar", true, 0, None, Some("^[a-z]+$"))
            .numberinput(
                "baz", "Baz", false, Some(-10.0), Some(10.0), Some(1.0))
    }

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
    fn test_form_build() {
        let form = testform();
        assert_eq!(form.fields.len(), 3);
    }

    #[test]
    fn test_form_validation_success() {
        let values = ValueMap::from_urlencoded(b"foo=1&bar=2&baz=3").unwrap();
        let form = testform()
            .validate_and_set(&values);
        println!("errors: {:?}", form.errors);
        assert_eq!(form.errors.len(), 0);
    }

    #[test]
    fn test_form_validation_missing_required() {
        let values = ValueMap::from_urlencoded(b"foo=1&baz=3").unwrap();
        let form = testform()
            .validate_and_set(&values);
        println!("errors: {:?}", form.errors);
        assert_eq!(form.errors.len(), 1);
        assert_eq!(form.errors.keys().collect::<Vec<&String>>(), vec!["bar"]);
        assert_eq!(
            form.errors.values().collect::<Vec<&String>>(),
            vec!["no value for required field"]);
    }
}
