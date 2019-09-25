use std::str::FromStr;
use std::collections::HashMap;

use serde::ser::{Serialize, Serializer, SerializeStruct};

use crate::error::{FormError, ValidationError};
use crate::value::{ValueMap, Value};
use crate::types::{
    Method, Element, InputType, SelectType, ButtonType, Constraint, Attr};

/// `HtmlForm` represents an HTML form. It is used to validate data (both on
/// the server and the client side) and to serialize forms in a consistent
/// manner (either as JSON or using a template language of choice). The
/// builder-style API makes it relatively easy to define forms:
///
/// ```rust
/// use htmlform::HtmlForm;
/// use htmlform::value::ValueMap;
/// use htmlform::types::{Method, InputType, Constraint, Attr};
///
/// fn main() {
///     // user input
///     let values = ValueMap::from_urlencoded(b"foo=bar").unwrap();
///     let form = HtmlForm::new(".", Method::Post)
///         .input(
///             InputType::Text, "foo", "Foo", true, None,
///             vec![], vec![]).unwrap()
///         .submit(None, "Submit", vec![]).unwrap()
///         .validate_and_set(values);
///     assert_eq!(form.errors.len(), 0);
///     assert_eq!(form.getone::<String>("foo").unwrap(), "bar");
/// }
/// ```
#[derive(Debug)]
pub struct HtmlForm<'a> {
    pub action: &'a str,
    pub method: Method,
    pub errors: HashMap<String, String>,
    pub fields: Vec<Field<'a>>,
}

impl <'a> HtmlForm<'a> {
    /// Instantiate an HtmlForm.
    pub fn new(action: &'a str, method: Method) -> HtmlForm<'a> {
        HtmlForm {
            action,
            method,
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
    /// use htmlform::HtmlForm;
    /// use htmlform::value::ValueMap;
    /// use htmlform::types::{Method, InputType, Constraint};
    ///
    /// fn main() {
    ///     let form = HtmlForm::new(".", Method::Post)
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
            if let Some(values) = values.values(&field.name) {
                if !field.element.multi() && values.len() > 1 {
                    self.errors.insert(
                        field.name.to_string(),
                        String::from("field can only have one value"));
                } else {
                    let values: Vec<&Value> = values.iter()
                        .map(|v| v)
                        .collect();
                    if let Err(e) = field.validate(&values) {
                        self.errors.insert(
                            field.name.to_string(), format!("{}", e));
                    }
                    field.set_values(values);
                }
            } else {
                // set default (empty) value
                field.empty();
                if field.required {
                    self.errors.insert(
                        field.name.to_string(),
                        String::from("no value for required field"));
                }
            }
        }
        self
    }

    /// Return a `Field` by name, or an error if there is not field by that
    /// name.
    pub fn field(self, name: &str) -> Result<Field<'a>, FormError> {
        for field in self.fields {
            if field.name == name {
                return Ok(field);
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
                return match &field.values {
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
                return match &field.values {
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

    /// Shortcut to create an `input` element, use this for non-collection
    /// fields (so not for `InputType::Radio` or `InputType::Checkbox`,
    /// for those see `choice_input()`). Returns self, so calls can
    /// be chained.
    pub fn input(
            self, input_type: InputType, name: &'a str, label: &'a str,
            required: bool, value: Option<&str>,
            constraints: Vec<Constraint<'a>>, attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let values = match value {
            Some(value) => Some(vec![value]),
            None => None,
        };
        self.element(
            Element::Input(input_type), name, label, required, values,
            &[], constraints, attributes)
    }

    /// Shortcut to create a set of `checkbox`es. Returns self, so calls
    /// can be chained.
    pub fn checkbox(
            self, name: &'a str, label: &'a str,
            required: bool, values: Option<Vec<&str>>,
            choices: &'a[(&'a str, &'a str)],
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        self.element(
            Element::Input(InputType::Checkbox), name, label, required,
            values, choices, vec![], attributes)
    }

    /// Shortcut to create a set of `radio` buttons. Returns self, so calls
    /// can be chained.
    pub fn radio(
            self, name: &'a str, label: &'a str,
            required: bool, value: Option<&str>,
            choices: &'a[(&'a str, &'a str)],
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let values = match value {
            Some(value) => Some(vec![value]),
            None => None,
        };
        self.element(
            Element::Input(InputType::Checkbox), name, label, required,
            values, choices, vec![], attributes)
    }

    /// Shortcut to create a text(-style) `input` with `datalist`
    /// for auto-fill suggestions. Returns self, so calls can be chained.
    pub fn datalist_input(
            self, input_type: InputType, name: &'a str, label: &'a str,
            required: bool, value: Option<&str>,
            datalist: &'a[(&'a str, &'a str)],
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        match input_type {
            InputType::Password |
            InputType::Radio |
            InputType::Checkbox |
            InputType::File |
            InputType::Hidden |
            InputType::Button |
            InputType::Submit |
            InputType::Reset => {
                return Err(FormError::new("invalid input type"));
            },
            _ => (),
        }
        let values = match value {
            Some(value) => Some(vec![value]),
            None => None,
        };
        self.element(
            Element::Input(input_type), name, label, required, values,
            datalist, vec![], attributes)
    }

    pub fn hidden(
            self, name: &'a str, value: Option<&str>,
            required: bool, constraints: Vec<Constraint<'a>>,
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let values = match value {
            Some(value) => Some(vec![value]),
            None => None,
        };
        self.element(
            Element::Input(InputType::Hidden), name, "", required, values,
            &[], constraints, attributes)
    }

    /// Shortcut to create a `textarea` without validation. Returns self,
    /// so calls can be chained.
    pub fn textarea(
            self, name: &'a str, label: &'a str, required: bool,
            value: Option<&str>, attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let values = match value {
            Some(value) => Some(vec![value]),
            None => None,
        };
        self.element(
            Element::Textarea, name, label, required, values,
            &[], vec![], attributes)
    }

    /// Shortcut to create a `select` dropdown. Returns self, so calls can
    /// be chained.
    pub fn select(
            self, name: &'a str, label: &'a str, multi: bool,
            required: bool, values: Option<Vec<&str>>,
            choices: &'a[(&'a str, &'a str)],
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let element = Element::Select(
            if multi {
                SelectType::Single
            } else {
                SelectType::Multi
            });
        self.element(
            element, name, label, required, values, choices, vec![],
            attributes)
    }

    /// Shortcut to create a `submit` button. Returns self, so calls can be
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
            &[], vec![], attributes)
    }

    /// Shortcut to create a `reset` button. Returns self, so calls can be
    /// chained.
    pub fn reset(
            self, label: &'a str, attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        self.element(
            Element::Input(InputType::Submit), "", label, false, None,
            &[], vec![], attributes)
    }

    /// Shortcut to create a `button` element. Returns self, so calls can be
    /// chained.
    pub fn button(
            self, button_type: ButtonType, name: &'a str, label: &'a str,
            value: Option<&str>,
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let values = match value {
            Some(value) => Some(vec![value]),
            None => None,
        };
        self.element(
            Element::Button(button_type), name, label, false, values,
            &[], vec![], attributes)
    }

    /// Create an field of any type. This is similar to Field::new(),
    /// but some checks and conversions are performed. Returns self, so
    /// calls can be chained.
    pub fn element(
            mut self, element: Element, name: &'a str, label: &'a str,
            required: bool, values: Option<Vec<&str>>,
            choices: &'a[(&'a str, &'a str)],
            constraints: Vec<Constraint<'a>>,
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let values = match values {
            Some(values) => {
                let values: Vec<Value> = values.iter()
                    .map(|v| Value::new(v))
                    .collect();
                for value in values.iter() {
                    if let Err(e) = element.validate(&value) {
                        return Err(FormError::new(&e.to_string()));
                    }
                }
                Some(values)
            },
            None => None,
        };
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
        let mut s = serializer.serialize_struct("Field", 4)?;
        s.serialize_field("action", &self.action)?;
        s.serialize_field("method", &self.method.attrvalue())?;
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
    choices: &'a[(&'a str, &'a str)],
    values: Option<Vec<Value>>,
    attributes: Vec<Attr<'a>>,
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
            required: bool, values: Option<Vec<Value>>,
            choices: &'a[(&'a str, &'a str)],
            constraints: Vec<Constraint<'a>>, attributes: Vec<Attr<'a>>)
            -> Field<'a> {
        Field {
            name,
            label,
            element,
            required,
            choices,
            constraints,
            attributes,
            values,
        }
    }

    /// Returns a `Vec` of 0 or more non-empty values from self.values.
    pub fn values(&self) -> Vec<Value> {
        match &self.values {
            None => Vec::new(),
            Some(value) =>
                value.iter()
                    .filter(|value| value.as_string() != "")
                    .map(|value| Value::new(&value.as_string()))
                    .collect(),
        }
    }

    /// Validate a set of values
    ///
    /// Note that this assumes `values` contains the correct amount of
    /// non-empty values for this `Field`, so dealing with errors
    /// regarding `self.required` and amount of values should be done before
    /// this method is called.
    ///
    /// Generally, this method is not called directly, but indirectly by
    /// `HtmlForm`'s `validate_and_set()`.
    pub fn validate(&self, values: &[&Value])
            -> Result<(), ValidationError> {
        // validate choices, but only for certain elements (on other
        // elements, choices are (optional) suggestions, usually to
        // populate the `datalist` element for auto-fill).
        match self.element {
            Element::Input(InputType::Checkbox) |
            Element::Select(SelectType::Multi) => {
                let choicevalues: Vec<&str> = self.choices.iter()
                    .map(|(value, _)| {
                        *value
                    })
                    .collect();
                for value in values {
                    let value_str = value.as_string();
                    if !choicevalues.contains(&value_str.as_str()) {
                        return Err(ValidationError::new("invalid choice"));
                    }
                    let split: Vec<&[&Value]> = values
                        .split(|v| v == value)
                        .collect();
                    if split.len() > 2 {
                        return Err(
                            ValidationError::new(
                                "value provided more than once"));
                    }
                }
            },
            _ => (),
        }
        for value in values {
            self.element.validate(&value)?;
            for constraint in self.constraints.iter() {
                constraint.validate(&value)?;
            }
        }
        Ok(())
    }

    /// Clear the `Field`'s values.
    pub fn empty(&mut self) {
        self.values = Some(Vec::new());
    }

    /// Set the `Field`'s values.
    pub fn set_values(&mut self, values: Vec<&Value>) {
        let mut clone = Vec::new();
        for value in values {
            clone.push(Value::new(&value.as_string()));
        }
        self.values = Some(clone);
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
        s.serialize_field("multi", &self.element.multi())?;
        s.serialize_field("choices", &self.choices)?;

        // attributes combines self.constraints and self.attributes
        let mut attributesmap = HashMap::new();
        for constraint in &self.constraints {
            if let Some((name, value)) = constraint.attrpair() {
                attributesmap.insert(name, value);
            }
        }
        for attribute in self.attributes.iter() {
            let (name, value) = attribute.attrpair();
            attributesmap.insert(name, value);
        }
        s.serialize_field("attributes", &attributesmap)?;

        // value is a single value (or None) if self.element.multi() == false,
        // else it's a list
        match &self.element.multi() {
            true => {
                s.serialize_field("value", &self.values)?;
            },
            false => {
                match &self.values {
                    Some(value) => {
                        let strvalue = if value.len() == 1 {
                            match self.element {
                                Element::Input(InputType::Password) =>
                                    String::new(),
                                _ => value[0].as_string(),
                            }
                        } else {
                            String::new()
                        };
                        s.serialize_field("value", &strvalue)?;
                    },
                    None => {
                        s.serialize_field("value", &self.values)?;
                    },
                }
            },
        }
        s.end()
    }
}
