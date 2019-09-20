use std::fmt;
use std::str::FromStr;
use std::error::Error;
use std::ops::Deref;
use std::collections::HashMap;

use serde::ser::{Serialize, Serializer, SerializeStruct};
use regex::Regex;

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
    Checkbox(SelectType),
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

pub enum SelectType {
    Single,
    Multi,
}

pub enum FormElement {
    Input(InputType),
    Textarea,
    Select(SelectType),
}

impl FormElement {
    fn fieldtype(&self) -> String {
        String::from(match &self {
            FormElement::Input(_) => "input",
            FormElement::Textarea => "textarea",
            FormElement::Select(_) => "select",
        })
    }

    fn subtype(&self) -> String {
        String::from(match &self {
            FormElement::Input(input_type) => match input_type {
                InputType::Text => "text",
                InputType::Password => "password",
                InputType::Radio => "radio",
                InputType::Checkbox(_) => "checkbox",
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
            FormElement::Input(InputType::Checkbox(SelectType::Multi)) =>
                true,
            FormElement::Select(SelectType::Multi) => true,
            _ => false,
        }
    }
}

pub enum Constraint {
    MinLength(usize),
    MaxLength(usize),
    MinValue(i32),
    MaxValue(i32),
    Pattern(String),
}

impl Constraint {
    pub fn validate(&self, formvalue: &FormValue)
            -> Result<(), ValidationError> {
        if formvalue.value.len() == 0 {
            return Ok(());
        }
        match self {
            Constraint::MinLength(min) => {
                let value: String = formvalue.value()?;
                if value.len() < *min {
                    return Err(ValidationError::new("value too short"));
                }
            },
            Constraint::MaxLength(max) => {
                let value: String = formvalue.value()?;
                if value.len() > *max {
                    return Err(ValidationError::new("value too short"));
                }
            },
            Constraint::MinValue(min) => {
                let value: i32 = formvalue.value()?;
                if value < *min {
                    return Err(ValidationError::new("value too small"));
                }
            },
            Constraint::MaxValue(max) => {
                let value: i32 = formvalue.value()?;
                if value > *max {
                    return Err(ValidationError::new("value too large"));
                }
            },
            Constraint::Pattern(pattern) => {
                let value: String = formvalue.value()?;
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
        let (key, value) = match self {
            Constraint::MinLength(value) =>
                ("minlength", format!("{}", value)),
            Constraint::MaxLength(value) =>
                ("maxlength", format!("{}", value)),
            Constraint::MinValue(value) =>
                ("min", format!("{}", value)),
            Constraint::MaxValue(value) =>
                ("max", format!("{}", value)),
            Constraint::Pattern(pattern) => ("pattern", pattern.clone()),
        };
        let mut s = serializer.serialize_struct("Constraint", 2)?;
        s.serialize_field("key", &key)?;
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
    fields: Vec<FormField>,
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
            mut self, values: &FormValueMap)
            -> Self {
        for field in self.fields.iter() {
            match values.get(&field.name) {
                Some(fieldvalues) => {
                    let mut error = String::new();
                    if field.required && values.len() == 0 {
                        error.push_str("no value for required field");
                    } else if !field.multi && values.len() > 1 {
                        error.push_str("only one value allowed");
                    } else if field.multi {
                        match field.validate_and_set_multi(fieldvalues) {
                            Err(e) => error.push_str(&format!("{}", e)),
                            Ok(()) => (),
                        }
                    } else {
                        match field.validate_and_set_single(
                                &fieldvalues[0]) {
                            Err(e) => error.push_str(&format!("{}", e)),
                            Ok(()) => (),
                        }
                    }
                    if &error.len() > &0 {
                        self.errors.insert(field.name.clone(), error);
                    }
                },
                None => {
                    if field.required {
                        self.errors.insert(
                            field.name.clone(),
                            String::from("no value provided"));
                    }
                }
            }
        }
        self
    }

    pub fn textinput(
            mut self, name: &str, label: &str, required: bool,
            minlength: usize, maxlength: Option<usize>, pattern: Option<&str>)
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
        self.fields.push(FormField::new(
            name, Label::new(label), FormElement::Input(InputType::Text),
            required, None, constraints));
        self
    }
}

pub struct FormField {
    name: String,
    label: Label,
    fieldtype: String,
    subtype: String,
    required: bool,
    multi: bool,
    choices: Vec<(String, Label)>,
    value: Option<FormValue>,
    multi_value: Option<Vec<FormValue>>,
    constraints: Vec<Constraint>,
}

impl FormField {
    fn new(name: &str, label: Label, element: FormElement, required: bool,
            choices: Option<Vec<(String, Label)>>,
            constraints: Vec<Constraint>)
            -> FormField {
        FormField {
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
            value: None,
            multi_value: None,
        }
    }

    fn validate_and_set_single(&self, value: &FormValue)
            -> Result<(), ValidationError> {
        for constraint in self.constraints.iter() {
            constraint.validate(value)?;
        }
        Ok(())
    }

    fn validate_and_set_multi(
            &self, values: &Vec<FormValue>)
            -> Result<(), ValidationError> {
        for constraint in self.constraints.iter() {
            for value in values.iter() {
                constraint.validate(&value)?;
            }
        }
        Ok(())
    }
}

impl Serialize for FormField {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer {
        let mut s = serializer.serialize_struct("FormField", 9)?;
        s.serialize_field("name", &self.name)?;
        s.serialize_field("label", &self.label)?;
        s.serialize_field("fieldtype", &self.fieldtype)?;
        s.serialize_field("subtype", &self.subtype)?;
        s.serialize_field("required", &self.required)?;
        s.serialize_field("multi", &self.multi)?;
        s.serialize_field("choices", &self.choices)?;
        s.serialize_field("constraints", &self.constraints)?;
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

pub struct FormValueMap {
    values: HashMap<String, Vec<FormValue>>,
}

impl Deref for FormValueMap {
    type Target = HashMap<String, Vec<FormValue>>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

pub struct FormValue {
    value: String,
}

impl FormValue {
    fn value<T>(&self) -> Result<T, ValidationError>
            where T: FromStr {
        match self.value.parse() {
            Ok(value) => Ok(value),
            Err(_) => Err(ValidationError::new("invalid value")),
        }
    }
}

// Deref
impl Deref for FormValue {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl Serialize for FormValue {
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
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
