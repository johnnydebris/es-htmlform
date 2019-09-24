use std::fmt;
use std::str::FromStr;
use std::error::Error;
use std::ops::Deref;
use std::collections::HashMap;

use serde::ser::{Serialize, Serializer};

use crate::ValidationError;

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

/// Decode url-encoded bytes to a UTF-8 `String`.
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

/// A set of form values. Note that values are always stored as lists of
/// strings, similar to how urlencoded form data is treated (no type
/// information, any field may appear more than once and there doesn't need
/// to be a value).
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
                v.0 != ""
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

/// A single form value, stored as `String`. May be empty in some cases,
/// which results in the value being ignored in validation.
#[derive(Debug, PartialEq)]
pub struct Value(String);

impl Value {
    pub fn new(value: &str) -> Value {
        Value(value.to_string())
    }

    pub fn as_string(&self) -> String {
        self.0.clone()
    }

    pub fn parse<T>(&self) -> Result<T, ValidationError>
            where T: FromStr {
        match self.0.parse() {
            Ok(value) => Ok(value),
            Err(_) => Err(ValidationError::new("invalid value")),
        }
    }
}

// Deref
impl Deref for Value {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer {
        serializer.serialize_str(&self.0)
    }
}
