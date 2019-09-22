//! Library to validate and generate HTML form data.
//!
//! ```rust
//! use regex::Regex;
//! use htmlform::{
//!     HtmlForm, ValueMap, InputType, Constraint as CS, Attr,
//!     FormError, ValidationError};
//!
//! fn userform() -> Result<HtmlForm, FormError> {
//!     Ok(HtmlForm::new()
//!         .input(
//!             InputType::Text, "username", "Username", true, None,
//!             vec![
//!                 CS::MinLength(5), CS::MaxLength(16),
//!                 CS::Pattern(r"^\w+$".to_string())],
//!             vec![])?
//!         .input(
//!             InputType::Text, "name", "Full name", true, None,
//!             vec![CS::MinLength(0), CS::MaxLength(64)], vec![])?
//!         .input(
//!             InputType::Password, "password", "Password", true, None,
//!             vec![
//!                 CS::MinLength(6), CS::MaxLength(64),
//!                 CS::Pattern(r"[^a-zA-Z]".to_string()),
//!                 CS::Func(Box::new(|ref value| {
//!                     let reg_number = Regex::new(r"\d").unwrap();
//!                     let reg_other = Regex::new(r"[^\w\s\d]").unwrap();
//!                     // we want at least 2 matches, so 3 parts
//!                     if !reg_number.is_match(value) ||
//!                             !reg_other.is_match(value) {
//!                         Err(ValidationError::new(
//!                             "must contain 1 number and 1 non-word char"))
//!                     } else {
//!                         Ok(())
//!                     }
//!                 })),
//!             ],
//!             vec![])?
//!         .input(
//!             InputType::Number, "age", "Age", true, None,
//!             vec![CS::MinNumber(18.0)],
//!             vec![
//!                 Attr::Step(1.0),
//!                 Attr::Any("id".to_string(), "age".to_string())])?
//!         .textarea("message", "Message", false, None, vec![])
//!         .submit(None, "Submit", vec![])
//!     )
//! }
//!
//! fn main() {
//!     let values = ValueMap::from_urlencoded(
//!         b"username=johnny&name=Johnny&password=foobar-123&age=46"
//!     ).unwrap();
//!     let form = userform().unwrap().validate_and_set(values);
//!
//!     println!("errors: {:?}", form.errors);
//!     assert_eq!(form.errors.len(), 0);
//! }
//! ```

use std::fmt;
use std::str::FromStr;
use std::error::Error;
use std::ops::Deref;
use std::collections::HashMap;
use std::sync::Mutex;

use serde::ser::{Serialize, Serializer, SerializeStruct};
use regex::Regex;

/// Error raised when `urldecode()` fails.
#[derive(Debug, PartialEq)]
pub struct UrlDecodingError {
    message: String,
}

impl UrlDecodingError {
    pub fn new(message: &str) -> UrlDecodingError {
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
    let plus: u8 = 43;
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
        } else if chr == plus {
            // stupid + signs in GET to replace spaces
            charcode = 32 // space
        } else {
            charcode = chr;
        }
        out.push(charcode);
    }
    Ok(std::str::from_utf8(&out)?.to_string())
}

#[derive(Debug)]
pub enum ContentType {
    Urlencoded,
    // XXX not yet...
    // Multipart,
    Json,
}

#[derive(Debug)]
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
    Button,
    Submit,
    Reset,
}

#[derive(Debug)]
pub enum Select {
    Single,
    Multi,
}

#[derive(Debug)]
pub enum Element {
    Input(InputType),
    Textarea,
    Select(Select),
    Button,
}

impl Element {
    pub fn fieldtype(&self) -> String {
        String::from(match &self {
            Element::Input(_) => "input",
            Element::Textarea => "textarea",
            Element::Select(_) => "select",
            Element::Button => "button",
        })
    }

    pub fn subtype(&self) -> String {
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
                InputType::Button => "button",
                InputType::Reset => "reset",
                InputType::Submit => "submit",
            },
            _ => "",
        })
    }

    pub fn multi(&self) -> bool {
        match self {
            Element::Input(InputType::Checkbox) =>
                true,
            Element::Select(Select::Multi) => true,
            _ => false,
        }
    }
}

/// Constraints on `Field` values, perform validation.
///
/// All of the constraints cause server-side validation to be performed,
/// all except `Constraint::Func` should - assuming they're serialized
/// properly - result in client-side validation.
pub enum Constraint {
    MinLength(usize),
    MaxLength(usize),
    MinNumber(f64),
    MaxNumber(f64),
    Pattern(String),
    Func(Box<Fn(&Value) -> Result<(), ValidationError>>),
}

impl Constraint {
    /// Validate a single, non-empty `Value`.
    pub fn validate(&self, formvalue: &Value)
            -> Result<(), ValidationError> {
        match self {
            Constraint::MinLength(min) => {
                let value: String = formvalue.parse()?;
                if value.len() < *min {
                    return Err(ValidationError::new("value too short"));
                }
            },
            Constraint::MaxLength(max) => {
                let value: String = formvalue.parse()?;
                if value.len() > *max {
                    return Err(ValidationError::new("value too long"));
                }
            },
            Constraint::MinNumber(min) => {
                let value: f64 = formvalue.parse()?;
                if value < *min {
                    return Err(ValidationError::new("value too low"));
                }
            },
            Constraint::MaxNumber(max) => {
                let value: f64 = formvalue.parse()?;
                if value > *max {
                    return Err(ValidationError::new("value too high"));
                }
            },
            Constraint::Pattern(pattern) => {
                let value: String = formvalue.parse()?;
                let reg = match Regex::new(pattern) {
                    Ok(reg) => reg,
                    Err(_) => return Err(
                        ValidationError::new("invalid pattern")),
                };
                if !reg.is_match(&value) {
                    return Err(
                        ValidationError::new("pattern did not match"));
                }
            },
            Constraint::Func(validator) => {
                validator(&formvalue)?;
            },
        }
        Ok(())
    }

    /// Returns the name and value of the HTML attribute of the Constraint.
    ///
    /// Returns None for `Constraint::Func`, as that is only functional on
    /// the server side.
    pub fn attrpair(&self) -> Option<(String, String)> {
        match self {
            Constraint::MinLength(min) =>
                Some((String::from("minlength"), min.to_string())),
            Constraint::MaxLength(max) =>
                Some((String::from("maxlength"), max.to_string())),
            Constraint::MinNumber(min) =>
                Some((String::from("min"), min.to_string())),
            Constraint::MaxNumber(max) =>
                Some((String::from("max"), max.to_string())),
            Constraint::Pattern(pattern) =>
                Some((String::from("pattern"), pattern.clone())),
            Constraint::Func(_) => None,
        }
    }

    fn allowed_on(&self, element: &Element) -> bool {
        match self {
            Constraint::MinLength(_) => true,
            Constraint::MaxLength(_) => true,
            Constraint::MinNumber(_) => match element {
                Element::Input(InputType::Number) => true,
                _ => false,
            },
            Constraint::MaxNumber(_) => match element {
                Element::Input(InputType::Number) => true,
                _ => false,
            },
            Constraint::Pattern(_) => match element {
                Element::Input(input_type) => match input_type {
                    InputType::Text |
                        InputType::Password |
                        InputType::Date |
                        InputType::Url |
                        InputType::Email |
                        InputType::Tel |
                        InputType::Search => true,
                    _ => false,
                },
                _ => false,
            },
            Constraint::Func(_) => true,
        }
    }
}

impl fmt::Debug for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::MinLength(len) => {
                write!(f, "Constraint::MinLength({})", len)
            },
            Constraint::MaxLength(len) => {
                write!(f, "Constraint::MaxLength({})", len)
            },
            Constraint::MinNumber(number) => {
                write!(f, "Constraint::MinNumber({})", number)
            },
            Constraint::MaxNumber(number) => {
                write!(f, "Constraint::MaxNumber({})", number)
            },
            Constraint::Pattern(pattern) => {
                write!(f, "Constraint::Pattern({})", pattern)
            },
            Constraint::Func(_) => {
                write!(f, "Constraint::Func(Fn)")
            },
        }
    }
}

#[derive(Debug)]
pub enum Attr {
    Any(String, String),
    Step(f64),
}

impl Attr {
    pub fn attrpair(&self) -> (String, String) {
        let (name, value) = match self {
            Attr::Any(name, value) => (name.deref(), value.clone()),
            Attr::Step(step) => ("step", step.to_string()),
        };
        (String::from(name), value)
    }
}

// XXX make translatable later
#[derive(Debug)]
pub struct Label(String);

impl Label {
    pub fn new(label: &str) -> Label {
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

#[derive(Debug)]
pub struct HtmlForm {
    pub errors: HashMap<String, String>,
    fields: Vec<Field>,
}

impl HtmlForm {
    /// Instantiate an HtmlForm.
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

    /// Validate the values in a `ValueMap` and set them on the form's fields
    ///
    /// This populates self.errors and the values in self.fields, the latter
    /// regardless of whether the values are valid (this to allow presenting
    /// a form with errors with the old values pre-filled).
    ///
    /// Example:
    ///
    /// ```rust
    /// use htmlform::{HtmlForm, ValueMap, InputType, Constraint};
    ///
    /// fn main() {
    ///     let form = HtmlForm::new()
    ///         .input(
    ///             InputType::Text, "foo", "Foo", true, None,
    ///             vec![Constraint::MinLength(5)], vec![]).unwrap()
    ///         .validate_and_set(
    ///             ValueMap::from_urlencoded(b"foo=bar").unwrap());
    ///    assert_eq!(form.errors.get("foo").unwrap(), "value too short");
    ///    assert_eq!(form.get::<String>("foo").unwrap(), vec!["bar"]);
    /// }
    /// ```
    pub fn validate_and_set(mut self, values: ValueMap)
            -> Self {
        for field in &mut self.fields {
            let non_empty = values.non_empty_values(&field.name);
            if non_empty.len() > 0 {
                if !field.multi && non_empty.len() > 1 {
                    self.errors.insert(
                        field.name.clone(),
                        String::from("field can only have one value"));
                } else {
                    match field.validate(&non_empty) {
                        Err(e) => {
                            self.errors.insert(
                                field.name.clone(), format!("{}", e));
                        },
                        Ok(()) => (),
                    }
                    field.set_values(non_empty);
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

    /// Return a list of values of a field. Returns an error when the
    /// field is not found, when the values can not be converted (parsed) or
    /// when the field has no value.
    pub fn get<T>(&self, name: &str) ->
            Result<Vec<T>, FormError>
            where T: FromStr {
        for field in &self.fields {
            if field.name == name {
                return match &field.value {
                    Some(_) => {
                        let mut converted: Vec<T> = Vec::new();
                        for value in field.values() {
                            converted.push(value.parse()?);
                        }
                        Ok(converted)
                    },
                    None => Err(
                        FormError::new("field has no value")),
                };
            }
        }
        Err(FormError::new("field not found"))
    }

    /// Return a single value for a field. Returns an error when the
    /// field is not found, when more than one value is found, when the
    /// value can not be converted (parsed) or when the field has no value.
    pub fn getone<T>(&self, name: &str) ->
            Result<Vec<T>, FormError>
            where T: FromStr {
        for field in &self.fields {
            if field.name == name {
                return match &field.value {
                    Some(_) => {
                        let mut converted: Vec<T> = Vec::new();
                        for value in field.values() {
                            converted.push(value.parse()?);
                        }
                        Ok(converted)
                    },
                    None => Err(
                        FormError::new("field has no value")),
                };
            }
        }
        Err(FormError::new("field not found"))
    }

    /// Set an attribute on a field.
    pub fn attr(&mut self, name: &str, attribute: Attr) ->
            Result<(), FormError> {
        for field in &self.fields {
            if field.name == name {
                field.attributes.lock().unwrap().push(attribute);
                return Ok(());
            }
        }
        Err(FormError::new("field not found"))
    }

    /// Shortcut to create an input element, use this for non-collection
    /// fields (so not for `InputType::Radio` or `InputType::Checkbox`).
    /// Returns self, so calls can be chained.
    pub fn input(
            mut self, input_type: InputType, name: &str, label: &str,
            required: bool, value: Option<Value>,
            constraints: Vec<Constraint>, attributes: Vec<Attr>)
            -> Result<Self, FormError> {
        let values = match value {
            Some(value) => Some(vec![value]),
            None => None,
        };
        let element = Element::Input(input_type);
        for constraint in constraints.iter() {
            if !constraint.allowed_on(&element) {
                return Err(FormError::new("constraint not allowed"));
            }
        }
        self.fields.push(Field::new(
            name, Label::new(label), element,
            required, None, values, constraints, attributes));
        Ok(self)
    }

    /// Shortcut to create a textarea without validation. Returns self, so
    /// calls can be chained.
    pub fn textarea(
            mut self, name: &str, label: &str, required: bool,
            value: Option<Value>, attributes: Vec<Attr>)
            -> Self {
        let values = match value {
            Some(value) => Some(vec![value]),
            None => None,
        };
        self.fields.push(Field::new(
            name, Label::new(label), Element::Textarea,
            required, None, values, vec![], attributes));
        self
    }

    /// Shortcut to create a submit button. Returns self, so
    /// calls can be chained.
    pub fn submit(
            mut self, name: Option<&str>, value: &str, attributes: Vec<Attr>)
            -> Self {
        let name = match name {
            Some(name) => name,
            None => "",
        };
        let values = vec![Value::new(value)];
        self.fields.push(Field::new(
            name, Label::new(""), Element::Input(InputType::Submit),
            false, None, Some(values), vec![], attributes));
        self
    }

    /// Create an field of any type, this is similar to Field::new(),
    /// but some checks and conversions are performed. Returns self, so calls
    /// can be chained.
    pub fn element(
            mut self, element: Element, name: &str, label: &str,
            required: bool, values: Vec<Value>, choices: &[(&str, &str)],
            constraints: Vec<Constraint>,
            attributes: Vec<Attr>)
            -> Self {
        let choices: Vec<(String, Label)> =
            choices.iter().map(|(name, value)| {
                (String::from(*name), Label::new(value))
            }).collect();
        self.fields.push(Field::new(
            name, Label::new(label), element, required, Some(choices),
            Some(values), constraints, attributes));
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

/// Represents a form field/element.
///
/// A `Field` can convert itself to HTML and to JSON (and other formats,
/// using serde) and can perform validation of its `Value`s based on
/// its `Constraint`s. Note that the `value` field always contains a `Vec`
/// of `Value`s, close to how HTML url encoding works (where every key can
/// appear more than once and every value is a string, or optional).
#[derive(Debug)]
pub struct Field {
    name: String,
    label: Label,
    fieldtype: String,
    subtype: String,
    required: bool,
    multi: bool,
    choices: Vec<(String, Label)>,
    value: Option<Vec<Value>>,
    attributes: Mutex<Vec<Attr>>,
    constraints: Vec<Constraint>,
}

impl Field {
    /// Initialize a `Field`.
    ///
    /// It is advised to use the builder-style methods on HtmlForm to
    /// instantiate, since those perform additional checks and prepare
    /// arguments.
    pub fn new(
            name: &str, label: Label, element: Element, required: bool,
            choices: Option<Vec<(String, Label)>>,
            value: Option<Vec<Value>>, constraints: Vec<Constraint>,
            attributes: Vec<Attr>)
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
            attributes: Mutex::new(attributes),
            value: value,
        }
    }

    /// Returns a `Vec` of 0 or more non-empty values from self.value.
    pub fn values(&self) -> Vec<Value> {
        match &self.value {
            None => Vec::new(),
            Some(value) =>
                value.iter()
                    .filter(|value| value.value != "")
                    .map(|value| Value {value: value.value.clone()})
                    .collect(),
        }
    }

    /// Validate a set of values
    ///
    /// Note that this assumes `values` contains the correct amount of
    /// non-empty values for this `Field`, so dealing with errors
    /// regarding `self.required` and `self.multi` should be done before
    /// this method is called.
    ///
    /// Generally, this method is not called directly, but indirectly by
    /// `HtmlForm`'s `validate_and_set()`.
    pub fn validate(&self, values: &Vec<&Value>)
            -> Result<(), ValidationError> {
        for constraint in self.constraints.iter() {
            for value in values.iter() {
                constraint.validate(&value)?;
            }
        }
        Ok(())
    }

    pub(crate) fn set_default_value(&mut self) {
        self.value = Some(Vec::new());
    }

    pub(crate) fn set_values(&mut self, values: Vec<&Value>) {
        let mut clone = Vec::new();
        for value in values {
            clone.push(Value {value: value.value.clone()});
        }
        self.value = Some(clone);
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

        // attributes combines self.constraints and self.attributes
        let mut attributesmap = HashMap::new();
        for constraint in &self.constraints {
            match constraint.attrpair() {
                Some((name, value)) => {
                    attributesmap.insert(name, value);
                },
                None => (),
            }
        }
        for attribute in self.attributes.lock().unwrap().iter() {
            let (name, value) = attribute.attrpair();
            attributesmap.insert(name, value);
        }
        s.serialize_field("attributes", &attributesmap)?;

        // value is a single value (or None) if self.multi == false,
        // else it's a list
        match &self.multi {
            true => {
                s.serialize_field("value", &self.value)?;
            },
            false => {
                match &self.value {
                    Some(value) => {
                        match value.len() {
                            1 => {
                                s.serialize_field("value", &value[0])?;
                            },
                            _ => {
                                s.serialize_field("value", &"")?;
                            },
                        };
                    },
                    None => {
                        s.serialize_field("value", &self.value)?;
                    },
                };
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
    pub fn non_empty_values(&self, key: &str) -> Vec<&Value> {
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
    pub fn new(value: &str) -> Value {
        Value {
            value: String::from(value),
        }
    }

    pub fn parse<T>(&self) -> Result<T, ValidationError>
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
pub struct FormError {
    message: String,
}

impl FormError {
    pub fn new(message: &str) -> FormError {
        FormError {
            message: String::from(message),
        }
    }
}

impl fmt::Display for FormError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for FormError {
    fn description(&self) -> &str {
        &self.message
    }
}

impl From<ValidationError> for FormError {
    fn from(e: ValidationError) -> FormError {
        FormError::new(&e.to_string())
    }
}

#[derive(Debug)]
pub struct ValidationError {
    message: String,
}

impl ValidationError {
    pub fn new(message: &str) -> ValidationError {
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
    use crate::{
        HtmlForm, Value, ValueMap, UrlDecodingError, InputType, Attr,
        Constraint, urldecode};

    fn testform() -> HtmlForm {
        HtmlForm::new()
            .input(
                InputType::Text, "foo", "Foo", true, None,
                vec![Constraint::MinLength(0), Constraint::MaxLength(10)],
                vec![]).unwrap()
            .input(
                InputType::Text, "bar", "Bar", true, None,
                vec![
                    Constraint::MinLength(0),
                    Constraint::MaxLength(10),
                    Constraint::Pattern("^[a-z]+$".to_string())],
                vec![]).unwrap()
            .input(
                InputType::Number, "baz", "Baz", false, None,
                vec![
                    Constraint::MinNumber(0.0),
                    Constraint::MaxNumber(10.0),
                ],
                vec![Attr::Step(0.1)]).unwrap()
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
    fn test_parse_urldecode_plus() {
        assert_eq!(urldecode(b"+").unwrap(), " ");
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
        let form = HtmlForm::new()
            .input(
                InputType::Text, "foo", "Foo", true, None,
                vec![Constraint::Func(Box::new(|_| Ok(())))], vec![],
            ).unwrap()
            .validate_and_set(values);
        assert_eq!(form.errors.len(), 0);
    }
}
