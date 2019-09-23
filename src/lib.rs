//! Library to validate and generate HTML form data.
//!
//! ```rust
//! use regex::Regex;
//! use htmlform::{
//!     HtmlForm, ValueMap, InputType, Constraint as Cons, Attr,
//!     FormError, ValidationError};
//!
//! fn searchform() -> Result<HtmlForm<'static>, FormError> {
//!     Ok(HtmlForm::new()
//!         .input(
//!             InputType::Text, "q", "Search", true, None, vec![], vec![])?
//!         .submit(None, "Search", vec![])?)
//! }
//!
//! fn userform() -> Result<HtmlForm<'static>, FormError> {
//!     Ok(HtmlForm::new()
//!         .input(
//!             InputType::Text, "username", "Username", true, None,
//!             vec![
//!                 Cons::MinLength(5), Cons::MaxLength(16),
//!                 Cons::Pattern(r"^\w+$")],
//!             vec![])?
//!         .input(
//!             InputType::Text, "name", "Full name", true, None,
//!             vec![Cons::MinLength(0), Cons::MaxLength(64)], vec![])?
//!         .input(
//!             InputType::Password, "password", "Password", true, None,
//!             vec![
//!                 Cons::MinLength(6), Cons::MaxLength(64),
//!                 Cons::Pattern(r"(\d.*[^\w\s\d]|[^\w\s\d].*\d)"),
//!                 Cons::Func(Box::new(|value| {
//!                     match value.as_string().as_str() {
//!                         "foobar-123" =>
//!                             Err(ValidationError::new(
//!                                 "password too simple!")),
//!                         _ => Ok(()),
//!                     }
//!                 })),
//!             ],
//!             vec![Attr::Title(
//!                 "Must contain 1 number and 1 non-word character")])?
//!         .input(
//!             InputType::Number, "age", "Age", true, None,
//!             vec![Cons::MinNumber(18.0)],
//!             vec![Attr::Step(1.0), Attr::Any("id", "age")])?
//!         .textarea("message", "Message", false, None, vec![])?
//!         .submit(None, "Submit", vec![])?
//!     )
//! }
//!
//! fn main() {
//!     // simple form with 1 field
//!     let values = ValueMap::from_urlencoded(b"q=foo").unwrap();
//!     let form = searchform().unwrap().validate_and_set(values);
//!
//!     assert_eq!(form.errors.len(), 0);
//!     assert_eq!(form.getone::<String>("q").unwrap(), "foo");
//!     assert_eq!(
//!         serde_json::to_string(&form).unwrap(),
//!         concat!(
//!             r#"{"errors":{},"fields":["#,
//!             r#"{"name":"q","label":"Search","fieldtype":"input","#,
//!             r#""subtype":"text","required":true,"multi":false,"#,
//!             r#""choices":[],"attributes":{},"value":"foo"},"#,
//!             r#"{"name":"","label":"Search","fieldtype":"input","#,
//!             r#""subtype":"submit","required":false,"multi":false,"#,
//!             r#""choices":[],"attributes":{},"value":""}]}"#));
//!
//!     // more elaborate example, with validation (both client- and
//!     // server-side) and custom attributes
//!     let values = ValueMap::from_urlencoded(
//!         b"username=johnny&name=Johnny&password=foobar-123&age=46"
//!     ).unwrap();
//!     let form = userform().unwrap().validate_and_set(values);
//!
//!     assert_eq!(form.errors.len(), 1);
//!     assert_eq!(
//!         form.errors.get("password").unwrap(), "password too simple!");
//!     assert_eq!(form.getone::<String>("username").unwrap(), "johnny");
//!     assert_eq!(form.getone::<String>("password").unwrap(), "foobar-123");
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

/// Error to denote `urldecode()` fails.
#[derive(Debug, PartialEq)]
pub struct UrlDecodingError<'a> {
    message: &'a str,
}

impl <'a> UrlDecodingError<'a> {
    pub fn new(message: &'a str) -> UrlDecodingError {
        UrlDecodingError {
            message: message,
        }
    }
}

impl <'a> fmt::Display for UrlDecodingError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl <'a> Error for UrlDecodingError<'a> {
    fn description(&self) -> &str {
        &self.message
    }
}

impl <'a> From<std::str::Utf8Error> for UrlDecodingError<'a> {
    fn from(_e: std::str::Utf8Error) -> UrlDecodingError<'a> {
        UrlDecodingError {
            message: "invalid encoding error sequence",
        }
    }
}

impl <'a> From<hex::FromHexError> for UrlDecodingError<'a> {
    fn from(_e: hex::FromHexError) -> UrlDecodingError<'a> {
        UrlDecodingError {
            message: "invalid encoding sequence",
        }
    }
}

impl <'a> From<std::num::ParseIntError> for UrlDecodingError<'a> {
    fn from(_e: std::num::ParseIntError) -> UrlDecodingError<'a> {
        UrlDecodingError {
            message: "invalid encoding character",
        }
    }
}

pub fn urldecode(input: &[u8]) -> Result<String, UrlDecodingError<'static>> {
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
            // depicting 2 character a hexdecimal number when combined,
            // which form the ascii value of 1 char (so another u8, unicode
            // is encoded as multiple u8s in a row, each with a separate %,
            // so if we handle the chars 1 at a time, we should end up with
            // a valid utf-8 sequence, assuming the character encoding is
            // utf-8 (XXX and what if it isn't?))
            charcode = hex::decode(
                &format!(
                    "{}{}", input[i] as char, input[i + 1] as char))?[0];
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
pub enum Element {
    Input(InputType),
    Textarea,
    Select(Select),
    Button,
}

impl Element {
    pub fn fieldtype(&self) -> &'static str {
        match &self {
            Element::Input(_) => "input",
            Element::Textarea => "textarea",
            Element::Select(_) => "select",
            Element::Button => "button",
        }
    }

    pub fn subtype(&self) -> &'static str {
        match &self {
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
        }
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

/// Constraints on `Field` values, perform validation.
///
/// All of the constraints cause server-side validation to be performed,
/// all except `Constraint::Func` should - assuming they're serialized
/// properly - result in client-side validation.
pub enum Constraint<'a> {
    MinLength(usize),
    MaxLength(usize),
    MinNumber(f64),
    MaxNumber(f64),
    Pattern(&'a str),
    Func(Box<Fn(&Value) -> Result<(), ValidationError>>),
}

impl <'a> Constraint<'a> {
    /// Validate a single, non-empty `Value`.
    pub fn validate(&self, formvalue: &Value)
            -> Result<(), ValidationError> {
        match self {
            Constraint::MinLength(min) => {
                let value = formvalue.as_string();
                if value.len() < *min {
                    return Err(ValidationError::new("value too short"));
                }
            },
            Constraint::MaxLength(max) => {
                let value = formvalue.as_string();
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
                let value = formvalue.as_string();
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
                Some((String::from("pattern"), pattern.to_string())),
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

impl <'a> fmt::Debug for Constraint<'a> {
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
pub enum Attr<'a> {
    Any(&'a str, &'a str),
    Step(f64),
    Placeholder(&'a str),
    Title(&'a str),
}

impl <'a> Attr<'a> {
    pub fn attrpair(&self) -> (String, String) {
        let (name, value) = match self {
            Attr::Any(name, value) => (name.deref(), value.to_string()),
            Attr::Step(step) => ("step", step.to_string()),
            Attr::Placeholder(label) =>
                ("placeholder", label.to_string()),
            Attr::Title(label) => ("title", label.to_string())
        };
        (String::from(name), value)
    }

    fn allowed_on(&self, element: &Element) -> bool {
        match self {
            Attr::Any(_, _) => true,
            Attr::Step(_) => match element {
                Element::Input(InputType::Number) => true,
                _ => false,
            },
            Attr::Placeholder(_) => true,
            Attr::Title(_) => true,
        }
    }
}

#[derive(Debug)]
pub struct HtmlForm<'a> {
    pub errors: HashMap<String, String>,
    pub fields: Vec<Field<'a>>,
}

impl <'a> HtmlForm<'a> {
    /// Instantiate an HtmlForm.
    pub fn new() -> HtmlForm<'a> {
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

    /// Validate the values in a `ValueMap` and save them on the form
    ///
    /// This populates self.errors and the values in self.fields, the latter
    /// regardless of whether the values are valid (this to allow presenting
    /// a form with errors with the old values pre-filled - note that
    /// password fields are emptied on serialization).
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
        self.errors.drain();
        for field in &mut self.fields {
            let non_empty = values.non_empty_values(&field.name);
            if non_empty.len() > 0 {
                if !field.multi && non_empty.len() > 1 {
                    self.errors.insert(
                        field.name.to_string(),
                        String::from("field can only have one value"));
                } else {
                    match field.validate(&non_empty) {
                        Err(e) => {
                            self.errors.insert(
                                field.name.to_string(), format!("{}", e));
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
                            field.name.to_string(),
                            String::from("no value for required field"));
                    },
                    false => (),
                }
            }
        }
        self
    }

    pub fn field(&self, name: &str) -> Result<&Field<'a>, FormError> {
        for field in &self.fields {
            if field.name == name {
                return Ok(&field);
            }
        }
        Err(FormError::new("no such field"))
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
            Result<T, FormError>
            where T: FromStr {
        for field in &self.fields {
            if field.name == name {
                return match &field.value {
                    Some(_) => {
                        let values = field.values();
                        if values.len() == 1 {
                            Ok(values[0].parse()?)
                        } else {
                            Err(FormError::new(
                                "field has more than one value"))
                        }
                    },
                    None => Err(
                        FormError::new("field has no value")),
                };
            }
        }
        Err(FormError::new("field not found"))
    }

    /// Set an attribute on a field.
    pub fn attr(&mut self, name: &str, attribute: Attr<'a>) ->
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
            self, input_type: InputType, name: &'a str, label: &'a str,
            required: bool, value: Option<Value>,
            constraints: Vec<Constraint<'a>>, attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let values = match value {
            Some(value) => Some(vec![value]),
            None => None,
        };
        self.element(
            Element::Input(input_type), name, label, required, values,
            vec![], constraints, attributes)
    }

    /// Shortcut to create a textarea without validation. Returns self, so
    /// calls can be chained.
    pub fn textarea(
            self, name: &'a str, label: &'a str, required: bool,
            value: Option<Value>, attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let values = match value {
            Some(value) => Some(vec![value]),
            None => None,
        };
        self.element(
            Element::Textarea, name, label, required, values,
            vec![], vec![], attributes)
    }

    /// Shortcut to create a submit button. Returns self, so calls can be
    /// chained.
    pub fn submit(
            self, name: Option<&'a str>, label: &'a str,
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let name = match name {
            Some(name) => name,
            None => "",
        };
        self.element(
            Element::Input(InputType::Submit), name, label, false, None,
            vec![], vec![], attributes)
    }

    /// Shortcut to create a reset button. Returns self, so calls can be
    /// chained.
    pub fn reset(
            self, label: &'a str, attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        self.element(
            Element::Input(InputType::Submit), "", label, false, None,
            vec![], vec![], attributes)
    }

    /// Shortcut to create a select dropdown. Returns self, so calls can be
    /// chained.
    pub fn select(
            self, name: &'a str, label: &'a str, multi: bool,
            required: bool, value: Option<Vec<Value>>,
            choices: Vec<(&'a str, &'a str)>,
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let element = Element::Select(
            match multi {
                false => Select::Single,
                true => Select::Multi,
            });
        self.element(
            element, name, label, required, value, choices, vec![],
            attributes)
    }


    /// Create an field of any type, this is similar to Field::new(),
    /// but some checks and conversions are performed. Returns self, so
    /// calls can be chained.
    pub fn element(
            mut self, element: Element, name: &'a str, label: &'a str,
            required: bool, values: Option<Vec<Value>>,
            choices: Vec<(&'a str, &'a str)>,
            constraints: Vec<Constraint<'a>>,
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        for constraint in constraints.iter() {
            if !constraint.allowed_on(&element) {
                return Err(FormError::new("constraint not allowed"));
            }
        }
        for attribute in attributes.iter() {
            if !attribute.allowed_on(&element) {
                return Err(FormError::new("attribute not allowed"));
            }
        }
        self.fields.push(Field::new(
            name, label, element, required, values, choices,
            constraints, attributes));
        Ok(self)
    }
}

impl <'a> Serialize for HtmlForm<'a> {
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
/// A `Field` can convert itself to JSON (and other formats, using
/// `serde`) and can perform validation of its `Value`s based on
/// its `Constraint`s. Note that the `value` field always contains a `Vec`
/// of `Value`s, close to how HTML url encoding works (where every key can
/// appear more than once and every value is a string, or optional).
#[derive(Debug)]
pub struct Field<'a> {
    name: &'a str,
    label: &'a str,
    element: Element,
    required: bool,
    multi: bool,
    choices: Vec<(&'a str, &'a str)>,
    value: Option<Vec<Value>>,
    attributes: Mutex<Vec<Attr<'a>>>,
    constraints: Vec<Constraint<'a>>,
}

impl <'a> Field<'a> {
    /// Initialize a `Field`.
    ///
    /// It is advised to use the builder-style methods on HtmlForm to
    /// instantiate, since those perform additional checks and prepare
    /// arguments.
    pub fn new(
            name: &'a str, label: &'a str, element: Element,
            required: bool, value: Option<Vec<Value>>,
            choices: Vec<(&'a str, &'a str)>,
            constraints: Vec<Constraint<'a>>, attributes: Vec<Attr<'a>>)
            -> Field<'a> {
        let multi = element.multi();
        Field {
            name: name,
            label: label,
            element: element,
            required: required,
            multi: multi,
            choices: choices,
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

impl <'a> Serialize for Field<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer {
        let mut s = serializer.serialize_struct("Field", 9)?;
        s.serialize_field("name", &self.name)?;
        s.serialize_field("label", &self.label)?;
        s.serialize_field("fieldtype", &self.element.fieldtype())?;
        s.serialize_field("subtype", &self.element.subtype())?;
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
    pub fn new(_values: HashMap<String, Vec<Value>>) -> ValueMap {
        ValueMap {
            values: HashMap::new(),
        }
    }

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
        let map = ValueMap {
            values: values,
        };
        Ok(map)
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
            value: value.to_string(),
        }
    }

    pub fn as_string(&self) -> String {
        self.value.clone()
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

/// Returned on form definition errors
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

/// Returned on form validation errors
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

    fn testform() -> HtmlForm<'static> {
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
                    Constraint::Pattern("^[a-z]+$")],
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
        assert_eq!(urldecode(b"foo+bar").unwrap(), "foo bar");
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

    #[test]
    fn test_constraint_not_allowed() {
        let result = HtmlForm::new()
            .input(
                InputType::Color, "foo", "Foo", true, None,
                vec![Constraint::MaxNumber(5.0)], vec![]);
        assert!(result.is_err());
    }
}
