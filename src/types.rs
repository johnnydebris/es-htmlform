//! Enums representing HTML elements, attributes and values.
//!
//! These should be complete according to the HTML specifications, all
//! elements and all element-specific attributes should be represented,
//! in such a way that your form structure and values will always be
//! valid (assuming that you use the `HtmlForm` builder methods to set
//! up the form, else form structure validity is not checked). Note that
//! the values of the `Constraint` enum (mostly) represent HTML attributes
//! that cause client-side validation, when used server-side validation
//! is also performed. One exception is `Constraint::Func()`, which is
//! used for per-element server-side validation and is not serialized
//! as client-side attribute. Attributes in the `Attr` enum do not cause
//! value validation.

use std::fmt;
use std::ops::Deref;

use regex::Regex;

use crate::error::ValidationError;
use crate::value::Value;

// XXX use lazy_static for the regs
fn validate_date(date: &str) -> Result<(), ValidationError> {
    let reg_date = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    if !reg_date.is_match(date) {
        Err(ValidationError::new(&format!("Invalid date {}.", date)))
    } else {
        Ok(())
    }
}

fn validate_datetime(datetime: &str) -> Result<(), ValidationError> {
    let reg_datetime = Regex::new(
        r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}$").unwrap();
    if !reg_datetime.is_match(datetime) {
        Err(ValidationError::new(&format!("Invalid datetime {}.", datetime)))
    } else {
        Ok(())
    }
}

fn validate_time(time: &str) -> Result<(), ValidationError> {
    let reg_time = Regex::new(r"^\d{2}:\d{2}$").unwrap();
    if !reg_time.is_match(time) {
        Err(ValidationError::new(&format!("Invalid time {}.", time)))
    } else {
        Ok(())
    }
}

fn validate_email(email: &str) -> Result<(), ValidationError> {
    // rather naive reg, but it should catch most common issues and should
    // not lead to false negatives
    let reg_email = Regex::new(r"^\S+@\w[-\.\w]+\.\w{2,}$").unwrap();
    if !reg_email.is_match(email) {
        Err(ValidationError::new(
            &format!("Invalid email address {}.", email)))
    } else {
        Ok(())
    }
}

fn validate_url(url: &str) -> Result<(), ValidationError> {
    url::Url::parse(url)
        .map(|_| ())
        .map_err(|err| ValidationError::new(&format!("{}", err)))
}

/// Form methods, correspond to `method` attribute values (note that these
/// do not correspond to HTTP methods per se, see specs).
#[derive(Debug)]
pub enum Method {
    Get,
    Post,
    // The `dialog` method can be used for forms in a pop-up, closes
    // the pop-up on submit (see specs).
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

// XXX Note that we miss some elements, we may want to add support for
// fieldset w. legend (contains a list of fields, should be traversed on
// validation), optgroup (how? don't like the thought of nested choices?)
// and output (simple add with no validation, it seems?)
/// Form element types, each of which represent a different HTML element.
#[derive(Debug)]
pub enum Element {
    Input(InputType),
    Textarea,
    Select(SelectType),
    Button(ButtonType),
}

impl Element {
    /// Validate a value for an element type.
    pub fn validate(&self, formvalue: &Value)
            -> Result<(), ValidationError> {
        match self {
            Element::Input(InputType::Date) => {
                validate_date(&formvalue.to_string())
            },
            Element::Input(InputType::Time) => {
                validate_time(&formvalue.to_string())
            },
            Element::Input(InputType::DateTime) => {
                validate_datetime(&formvalue.to_string())
            },
            Element::Input(InputType::Email) => {
                validate_email(&formvalue.to_string())
            },
            Element::Input(InputType::Url) => {
                validate_url(&formvalue.to_string())
            },
            _ => Ok(())
        }
    }

    /// Return the element's name (nodeName), used by `HtmlForm` to fill
    /// its `element` attribute.
    pub fn element_name(&self) -> &'static str {
        match &self {
            Element::Input(_) => "input",
            Element::Textarea => "textarea",
            Element::Select(_) => "select",
            Element::Button(_) => "button",
        }
    }

    /// Return the element's type (`type` attribute), used by `HtmlForm` to
    /// fill its `type` attribute.
    pub fn element_type(&self) -> &'static str {
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
                InputType::Button => "button",
                InputType::Reset => "reset",
                InputType::Submit => "submit",
                InputType::Image => "image",
                InputType::Hidden => "hidden",
            },
            Element::Button(button_type) => match button_type {
                ButtonType::Submit => "submit",
                ButtonType::Reset => "reset",
                ButtonType::Button => "button",
            },
            _ => "",
        }
    }

    /// Return `true` for multi-selects and checkbox inputs, used by
    /// `HtmlForm` to fill its `multi` attribute.
    pub fn multi(&self) -> bool {
        matches!(self, Element::Input(InputType::Checkbox) |
                 Element::Select(SelectType::Multi))
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
    Button,
    Submit,
    Reset,
    Image,
    Hidden,
}

/// Value for `Element::Select()` to determine `select` element behaviour.
#[derive(Debug)]
pub enum SelectType {
    Single,
    Multi,
}

/// Value for `Element::Button()` to determine `button` element behaviour.
#[derive(Debug)]
pub enum ButtonType {
    Submit,
    Reset,
    Button,
}

/// Value for `Attr::Spellcheck()` to determine `textarea` spell checking
/// behaviour.
#[derive(Debug)]
pub enum Spellcheck {
    True,
    Default,
    False,
}

/// Value for `Attr::Wrap()` to determine `textarea` wrapping behaviour.
#[derive(Debug)]
pub enum Wrap {
    Hard,
    Soft,
    Off,
}

/// Value for `Attr::Autocomplete()`.
#[derive(Debug)]
pub enum Autocomplete {
    On,
    Off,
}

/// Constraints on `Field` values, perform validation.
///
/// All of the constraints cause server-side validation to be performed,
/// all except `Constraint::Func` should - assuming they're serialized
/// properly - result in client-side validation. HTML validity is checked
/// when adding the constraints to the fields using `HtmlForm`'s builder
/// methods.
pub enum Constraint<'a> {
    /// Constraint on most inputs.
    MinLength(usize),
    /// Constraint on most inputs.
    MaxLength(usize),
    /// Constraint on `number` and `range` inputs.
    MinNumber(f64),
    /// Constraint on `number` and `range` inputs.
    MaxNumber(f64),
    /// Constraint on `date` input.
    MinDate(&'a str),
    /// Constraint on `date` input.
    MaxDate(&'a str),
    /// Constraint on `datetime-local` input.
    MinDateTime(&'a str),
    /// Constraint on `datetime-local` input.
    MaxDateTime(&'a str),
    /// Constraint on `time` input.
    MinTime(&'a str),
    /// Constraint on `time` input.
    MaxTime(&'a str),
    /// Constraint on most `Element::Input` fields, causes regex pattern
    /// validation (both on the server and the client, note that the pattern
    /// therefore must execute correctly on both sides).
    Pattern(&'a str),
    /// Constraint on any field, is executed server-side only and not
    /// serialized.
    Func(Box<dyn Fn(&Value) -> Result<(), ValidationError>>),
}

impl <'a> Constraint<'a> {
    /// Validate a single, non-empty `Value`.
    pub fn validate(&self, formvalue: &Value)
            -> Result<(), ValidationError> {
        match self {
            Constraint::MinLength(min) => {
                let value = formvalue.as_string();
                if value.len() < *min {
                    return Err(ValidationError::new(
                        &format!(
                            "Must be at least {} characters long.",
                            min)));
                }
            },
            Constraint::MaxLength(max) => {
                let value = formvalue.as_string();
                if value.len() > *max {
                    return Err(ValidationError::new(
                        &format!(
                            "Can not be more than {} characters long.",
                            max)));
                }
            },
            Constraint::MinNumber(min) => {
                let value: f64 = formvalue.parse()?;
                if value < *min {
                    return Err(ValidationError::new(
                        &format!("Must be at least {}.", min)));
                }
            },
            Constraint::MaxNumber(max) => {
                let value: f64 = formvalue.parse()?;
                if value > *max {
                    return Err(ValidationError::new(
                        &format!("Can not be more than {}.", max)));
                }
            },
            Constraint::MinDate(min) => {
                let value = formvalue.to_string();
                if let Err(e) = validate_date(&value) {
                    return Err(e);
                }
                // somewhat nasty, but taking into account the (fixed)
                // format of the date, we can do char by char comparison
                // to determine whether the provided value is less than min
                for (i, chr) in value.as_bytes().iter().enumerate() {
                    if *chr < min.as_bytes()[i] {
                        return Err(ValidationError::new(
                            &format!(
                                "Date should be after {}.", min)));
                    }
                }
            },
            Constraint::MaxDate(max) => {
                let value = formvalue.to_string();
                if let Err(e) = validate_date(&value) {
                    return Err(e);
                }
                for (i, chr) in value.as_bytes().iter().enumerate() {
                    if *chr > max.as_bytes()[i] {
                        return Err(ValidationError::new(
                            &format!(
                                "Date can not be after {}.", max)));
                    }
                }
            },
            Constraint::MinDateTime(min) => {
                let value = formvalue.to_string();
                if let Err(e) = validate_datetime(&value) {
                    return Err(e);
                }
                for (i, chr) in value.as_bytes().iter().enumerate() {
                    if *chr < min.as_bytes()[i] {
                        return Err(ValidationError::new(
                            &format!(
                                "Date and time must be after {}",
                                min)));
                    }
                }
            },
            Constraint::MaxDateTime(max) => {
                let value = formvalue.to_string();
                if let Err(e) = validate_datetime(&value) {
                    return Err(e);
                }
                for (i, chr) in value.as_bytes().iter().enumerate() {
                    if *chr > max.as_bytes()[i] {
                        return Err(ValidationError::new(
                            &format!(
                                "Date and time can not be after {}.",
                                max)));
                    }
                }
            },
            Constraint::MinTime(min) => {
                let value = formvalue.to_string();
                if let Err(e) = validate_time(&value) {
                    return Err(e);
                }
                for (i, chr) in value.as_bytes().iter().enumerate() {
                    if *chr < min.as_bytes()[i] {
                        return Err(ValidationError::new(
                            &format!(
                                "Time must be after {}.", min)));
                    }
                }
            },
            Constraint::MaxTime(max) => {
                let value = formvalue.to_string();
                if let Err(e) = validate_time(&value) {
                    return Err(e);
                }
                for (i, chr) in value.as_bytes().iter().enumerate() {
                    if *chr > max.as_bytes()[i] {
                        return Err(ValidationError::new(
                            &format!(
                                "Time can not be after {}.", max)));
                    }
                }
            },
            Constraint::Pattern(pattern) => {
                let value = formvalue.as_string();
                let reg = match Regex::new(pattern) {
                    Ok(reg) => reg,
                    Err(_) => return Err(
                        ValidationError::new(
                            &format!("Invalid pattern {:?}.", value))),
                };
                if !reg.is_match(&value) {
                    return Err(
                        ValidationError::new(
                            &"Please match the format requested.".to_string()));
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
            Constraint::MinDate(min) |
            Constraint::MinTime(min) |
            Constraint::MinDateTime(min) =>
                Some((String::from("min"), min.to_string())),
            Constraint::MaxDate(max) |
            Constraint::MaxTime(max) |
            Constraint::MaxDateTime(max) =>
                Some((String::from("max"), max.to_string())),
            Constraint::Pattern(pattern) =>
                Some((String::from("pattern"), pattern.to_string())),
            Constraint::Func(_) => None,
        }
    }

    /// Return true if this `Constraint` is allowed on `element`,
    /// false otherwise.
    pub fn allowed_on(&self, element: &Element) -> bool {
        match self {
            Constraint::MinLength(_) |
            Constraint::MaxLength(_) => match element {
                Element::Textarea => true,
                Element::Input(input_type) => match input_type {
                    InputType::Radio |
                    InputType::Checkbox |
                    InputType::File |
                    InputType::Button |
                    InputType::Submit |
                    InputType::Reset |
                    InputType::Image |
                    InputType::Hidden => false,
                    _ => true,
                },
                _ => false,
            },
            Constraint::MinNumber(_) =>
                matches!(element, Element::Input(InputType::Number)),
            Constraint::MaxNumber(_) =>
                matches!(element, Element::Input(InputType::Number)),
            Constraint::MinDate(_) =>
                matches!(element, Element::Input(InputType::Date)),
            Constraint::MaxDate(_) =>
                matches!(element, Element::Input(InputType::Date)),
            Constraint::MinTime(_) =>
                matches!(element, Element::Input(InputType::Time)),
            Constraint::MaxTime(_) =>
                matches!(element, Element::Input(InputType::Time)),
            Constraint::MinDateTime(_) =>
                matches!(element, Element::Input(InputType::DateTime)),
            Constraint::MaxDateTime(_) =>
                matches!(element, Element::Input(InputType::DateTime)),
            Constraint::Pattern(_) => match element {
                Element::Textarea => true,
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
            Constraint::MinDate(number) => {
                write!(f, "Constraint::MinDate({})", number)
            },
            Constraint::MaxDate(number) => {
                write!(f, "Constraint::MaxDate({})", number)
            },
            Constraint::MinTime(number) => {
                write!(f, "Constraint::MinTime({})", number)
            },
            Constraint::MaxTime(number) => {
                write!(f, "Constraint::MaxTime({})", number)
            },
            Constraint::MinDateTime(number) => {
                write!(f, "Constraint::MinDateTime({})", number)
            },
            Constraint::MaxDateTime(number) => {
                write!(f, "Constraint::MaxDateTime({})", number)
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

// XXX missing 'form' attribute - should we add that?
/// An HTML attribute without validation behaviour. The list should be
/// complete and up-to-date with the latest HTML specifications. HTML
/// validity is checked when adding the attributes to the fields using
/// `HtmlForm`'s builder methods.
#[derive(Debug)]
pub enum Attr<'a> {
    /// Use to add any attribute to any element without validation.
    Any(&'a str, &'a str),

    // general attrs for most elements/inputs
    Id(&'a str),
    Title(&'a str),
    Placeholder(&'a str),
    Autocomplete(Autocomplete),
    Autofocus,
    Disabled,
    Readonly,
    Tabindex(i64),

    // 'step' for number, range, date, datetime, etc
    StepFloat(f64),
    StepInt(u64),

    // rendering hints for most inputs
    Size(u64),
    Width(u64),
    Height(u64),

    // specific to textarea
    Rows(u64),
    Cols(u64),
    Spellcheck(Spellcheck),
    Wrap(Wrap),

    // control form behaviour from an input, usually buttons (new to HTML 5)
    FormAction(&'a str),
    FormEnctype(&'a str),
    FormNoValidate,
    FormTarget(&'a str),
}

impl <'a> Attr<'a> {
    pub fn attrpair(&self) -> (String, String) {
        let (name, value) = match self {
            Attr::Any(name, value) => (name.deref(), value.to_string()),
            Attr::StepFloat(step) => ("step", step.to_string()),
            Attr::StepInt(step) => ("step", step.to_string()),
            Attr::Size(size) => ("size", size.to_string()),
            Attr::Width(width) => ("width", width.to_string()),
            Attr::Height(height) => ("height", height.to_string()),
            Attr::Rows(rows) => ("rows", rows.to_string()),
            Attr::Cols(cols) => ("cols", cols.to_string()),
            Attr::Spellcheck(wrap) =>
                ("wrap", match wrap {
                    Spellcheck::True => String::from("true"),
                    Spellcheck::Default => String::from("default"),
                    Spellcheck::False => String::from("false"),
                }),
            Attr::Wrap(wrap) =>
                ("wrap", match wrap {
                    Wrap::Hard => String::from("hard"),
                    Wrap::Soft => String::from("soft"),
                    Wrap::Off => String::from("off"),
                }),
            Attr::FormAction(formaction) =>
                ("formaction", formaction.to_string()),
            Attr::FormEnctype(formenctype) =>
                ("formenctype", formenctype.to_string()),
            Attr::FormNoValidate => (
                "formnovalidate", String::from("formnovalidate")),
            Attr::FormTarget(formtarget) =>
                ("formtarget", formtarget.to_string()),
            Attr::Id(id) => ("id", id.to_string()),
            Attr::Title(label) => ("title", label.to_string()),
            Attr::Placeholder(label) =>
                ("placeholder", label.to_string()),
            Attr::Autocomplete(autocomplete) =>
                ("autocomplete", match autocomplete {
                    Autocomplete::On => String::from("on"),
                    Autocomplete::Off => String::from("off"),
                }),
            Attr::Autofocus => ("autofocus", String::from("autofocus")),
            Attr::Disabled => ("disabled", String::from("disabled")),
            Attr::Readonly => ("readonly", String::from("readonly")),
            Attr::Tabindex(tabindex) => ("tabindex", tabindex.to_string()),
        };
        (String::from(name), value)
    }

    pub fn allowed_on(&self, element: &Element) -> bool {
        match element {
            Element::Input(input_type) => match self {
                Attr::Any(_, _) |
                Attr::Id(_) |
                // title makes no sense on some elements, but does seem
                // to be allowed on all...
                Attr::Title(_) => true,
                Attr::StepFloat(_) =>
                    matches!(input_type, InputType::Number),
                Attr::StepInt(_) => match input_type {
                    InputType::Number |
                    InputType::Date |
                    InputType::Time |
                    InputType::DateTime |
                    InputType::Month |
                    InputType::Week |
                    InputType::Range => true,
                    _ => false,
                },
                Attr::Width(_) |
                Attr::Height(_) => match input_type {
                    InputType::Image => true,
                    _ => false,
                },
                Attr::Rows(_) |
                Attr::Cols(_) |
                Attr::Spellcheck(_) |
                Attr::Wrap(_) => false,
                Attr::FormAction(_) |
                Attr::FormEnctype(_) |
                Attr::FormTarget(_) =>
                    matches!(input_type, InputType::Submit | InputType::Image),
                Attr::FormNoValidate =>
                    matches!(input_type, InputType::Submit),
                Attr::Placeholder(_) => match input_type {
                    InputType::Text |
                    InputType::Password |
                    InputType::Email |
                    InputType::Url |
                    InputType::Tel |
                    InputType::Search => true,
                    _ => false,
                },
                Attr::Autocomplete(_) => match input_type {
                    InputType::Text |
                    InputType::Email |
                    InputType::Url |
                    InputType::Tel |
                    InputType::Search |
                    InputType::Date |
                    InputType::DateTime |
                    InputType::Month | // XXX ?
                    InputType::Week | // XXX ?
                    InputType::Time | // XXX ?
                    InputType::Range |
                    InputType::Color => true,
                    _ => false,
                },
                Attr::Autofocus => match input_type {
                    InputType::Button |
                    InputType::Submit |
                    InputType::Reset |
                    InputType::Image |
                    InputType::Hidden => false,
                    _ => true,
                },
                Attr::Size(_) |
                Attr::Disabled |
                Attr::Readonly |
                Attr::Tabindex(_) => match input_type {
                    InputType::Hidden => false,
                    _ => true,
                },
            },
            Element::Textarea => match self {
                Attr::Any(_, _) |
                Attr::Id(_) |
                Attr::Title(_) |
                Attr::Placeholder(_) |
                Attr::Autocomplete(_) |
                Attr::Autofocus |
                Attr::Disabled |
                Attr::Readonly |
                Attr::Rows(_) |
                Attr::Cols(_) |
                Attr::Spellcheck(_) |
                Attr::Wrap(_) => true,
                _ => false,
            },
            Element::Select(_) => false,
            Element::Button(_) => false,
        }
    }
}
