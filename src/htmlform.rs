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
/// use es_htmlform::HtmlForm;
/// use es_htmlform::value::ValueMap;
/// use es_htmlform::types::{Method, InputType, Constraint, Attr};
///
///     // user input
///     let values = ValueMap::from_urlencoded(b"foo=bar").unwrap();
///     let mut form = HtmlForm::new(".", Method::Post)
///         .input(
///             InputType::Text, "foo", "Foo", true,
///             vec![], vec![]).unwrap()
///         .submit(None, "Submit", vec![]).unwrap();
///     form.update(&values, true);
///     assert_eq!(form.errors.len(), 0);
///     assert_eq!(form.get_string("foo").unwrap(), "bar");
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
    /// use es_htmlform::HtmlForm;
    /// use es_htmlform::value::ValueMap;
    /// use es_htmlform::types::{Method, InputType, Constraint};
    ///
    ///     let mut form = HtmlForm::new(".", Method::Post)
    ///         .input(
    ///             InputType::Text, "foo", "Foo", true,
    ///             vec![Constraint::MinLength(5)], vec![]).unwrap();
    ///     form.update(
    ///         &ValueMap::from_urlencoded(b"foo=bar").unwrap(), true);
    ///     assert_eq!(
    ///         form.errors.get("foo").unwrap(),
    ///         "Must be at least 5 characters long.");
    ///     assert_eq!(form.get::<String>("foo").unwrap(), vec!["bar"]);
    /// ```
    pub fn update(&mut self, values: &ValueMap, check_required: bool) {
        self.errors.drain();
        for field in &mut self.fields {
            if let Some(values) = values.values(&field.name) {
                if !field.element.multi() && values.len() > 1 {
                    self.errors.insert(
                        field.name.to_string(),
                        String::from("field can only have one value"));
                } else {
                    let values: Vec<&Value> = values.iter()
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
                if check_required && field.required {
                    self.errors.insert(
                        field.name.to_string(),
                        String::from("no value for required field"));
                }
            }
        }
    }

    /// Return a `Field` by name, or an error if there is not field by that
    /// name.
    pub fn field(self, name: &str) -> Result<Field<'a>, FormError> {
        for field in self.fields {
            if field.name == name {
                return Ok(field);
            }
        }
        Err(FormError::new(&format!("no field named {}", name)))
    }

    /// Return a list of values of a field, parsed to `T`. Returns an error
    /// when the field is not found, when the values can not be converted
    /// (parsed) or when the field has no value.
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
                    None => Err(FormError::new(
                        &format!("field {} has no value", name))),
                };
            }
        }
        Err(FormError::new(
            &format!("field {} not found", name)))
    }

    /// Return a single value for a field, parsed to `T`. Returns an error
    /// when the field is not found, when more than one value is found, when
    /// the value can not be converted (parsed) or when the field has no value.
    pub fn getone<T>(&self, name: &str) ->
            Result<T, FormError>
            where T: FromStr {
        for field in &self.fields {
            if field.name == name {
                return match &field.values {
                    Some(_) => {
                        let values = field.values();
                        match values.len() {
                            0 => {
                                Err(FormError::new(
                                    &format!(
                                        "field {} has no value",
                                        name)))
                            },
                            1 => {
                                Ok(values[0].parse()?)
                            },
                            _ => {
                                Err(FormError::new(
                                    &format!(
                                        "field {} has more than one value",
                                        name)))
                            },
                        }
                    },
                    None => Err(FormError::new(
                        &format!("field {} has no value", name))),
                };
            }
        }
        Err(FormError::new(&format!("field {} not found", name)))
    }

    /// Return a list of values of a field, as `String`s. Returns an error when
    /// the field is not found or when the field has no value.
    pub fn get_strings(&self, name: &str) -> Result<Vec<String>, FormError> {
        for field in &self.fields {
            if field.name == name {
                return match &field.values {
                    Some(values) => {
                        Ok(values.iter().map(|v| v.as_string()).collect())
                    },
                    None => Err(FormError::new(
                        &format!("field {} has no value", name))),
                };
            }
        }
        Err(FormError::new(
            &format!("field {} not found", name)))
    }

    /// Return a single value for a field as `String`. Returns an error
    /// when the field is not found, when more than one value is found,
    /// or when the field has no value.
    pub fn get_string(&self, name: &str) -> Result<String, FormError> {
        self.getone::<String>(name)
    }

    /// Shortcut to create an `input` element, use this for non-collection
    /// fields (so not for `InputType::Radio` or `InputType::Checkbox`,
    /// for those see `choice_input()`). Returns self, so calls can
    /// be chained.
    pub fn input(
            self, input_type: InputType, name: &'a str, label: &'a str,
            required: bool, constraints: Vec<Constraint<'a>>,
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        self.element(
            Element::Input(input_type), name, label, required, None,
            &[], constraints, attributes)
    }

    /// Shortcut to create a set of `checkbox`es. Returns self, so calls
    /// can be chained.
    pub fn checkbox(
            self, name: &'a str, label: &'a str,
            required: bool, choices: &'a[(&'a str, &'a str)],
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        self.element(
            Element::Input(InputType::Checkbox), name, label, required,
            None, choices, vec![], attributes)
    }

    /// Shortcut to create a set of `radio` buttons. Returns self, so calls
    /// can be chained.
    pub fn radio(
            self, name: &'a str, label: &'a str,
            required: bool, choices: &'a[(&'a str, &'a str)],
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        self.element(
            Element::Input(InputType::Radio), name, label, required,
            None, choices, vec![], attributes)
    }

    /// Shortcut to create a text(-style) `input` with `datalist`
    /// for auto-fill suggestions. Returns self, so calls can be chained.
    pub fn datalist_input(
            self, input_type: InputType, name: &'a str, label: &'a str,
            required: bool, datalist: &'a[(&'a str, &'a str)],
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
                return Err(FormError::new(
                    &format!(
                        "invalid input type {:?} for datalist input",
                        input_type)));
            },
            _ => (),
        }
        self.element(
            Element::Input(input_type), name, label, required, None,
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
            constraints: Vec<Constraint<'a>>, attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        self.element(
            Element::Textarea, name, label, required, None,
            &[], constraints, attributes)
    }

    /// Shortcut to create a `select` dropdown. Returns self, so calls can
    /// be chained.
    pub fn select(
            self, name: &'a str, label: &'a str, multi: bool,
            required: bool, choices: &'a[(&'a str, &'a str)],
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        let element = Element::Select(
            if multi {
                SelectType::Single
            } else {
                SelectType::Multi
            });
        self.element(
            element, name, label, required, None, choices, vec![],
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
            attributes: Vec<Attr<'a>>)
            -> Result<Self, FormError> {
        self.element(
            Element::Button(button_type), name, label, false, None,
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
                return Err(FormError::new(
                    &format!("constraint {:?} not allowed", constraint)));
            }
        }
        for attribute in attributes.iter() {
            if !attribute.allowed_on(&element) {
                return Err(FormError::new(
                    &format!("attribute {:?} not allowed", attribute)));
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
    /// `HtmlForm`'s `update()`.
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
                        return Err(ValidationError::new(
                            &format!(
                                "{} is not a valid choice.", value_str)));
                    }
                    let split: Vec<&[&Value]> = values
                        .split(|v| v == value)
                        .collect();
                    if split.len() > 2 {
                        return Err(
                            ValidationError::new(
                                &format!(
                                    "Value {} provided more than once.",
                                    value_str)));
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
        s.serialize_field("element", &self.element.element_name())?;
        s.serialize_field("type", &self.element.element_type())?;
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
