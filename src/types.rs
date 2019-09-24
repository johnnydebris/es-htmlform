use std::fmt;
use std::ops::Deref;

use regex::Regex;

use crate::error::ValidationError;
use crate::value::Value;

/// Methods, correspond to `method` attribute values (note that these
/// do not correspond to HTTP methods, see specs).
#[derive(Debug)]
pub enum Method {
    Get,
    Post,
    Dialog,
}

impl Method {
    pub fn attrvalue(&self) -> String {
        match &self {
            Method::Get => "get",
            Method::Post => "post",
            Method::Dialog => "dialog",
        }.to_string()
    }
}

/// Form element types, each of which results in a different HTML element.
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
            Element::Input(InputType::Checkbox) |
            Element::Select(Select::Multi) => true,
            _ => false,
        }
    }
}

/// Different input types, use with `Element::Input()`. Used as value of
/// the `type` attribute on an HTML `input` element.
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

/// Pass this to `Element::Select()` to determine selection behaviour.
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

    pub fn allowed_on(&self, element: &Element) -> bool {
        match self {
            Constraint::MinLength(_) |
            Constraint::MaxLength(_) => match element {
                Element::Input(_) => true,
                _ => false,
            },
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

/// An HTML attribute without validation behaviour.
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

    pub fn allowed_on(&self, element: &Element) -> bool {
        match self {
            Attr::Any(_, _) |
            Attr::Placeholder(_) |
            Attr::Title(_) => true,
            Attr::Step(_) => match element {
                Element::Input(InputType::Number) => true,
                _ => false,
            },
        }
    }
}
