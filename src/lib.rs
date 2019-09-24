//! Library to validate and generate HTML forms.
//!
//! ```rust
//! use htmlform::{
//!     HtmlForm, ValueMap, InputType, Method, Constraint as Cons, Attr,
//!     FormError, ValidationError};
//!
//! // simple form with 1 field
//! fn searchform() -> Result<HtmlForm<'static>, FormError> {
//!     Ok(HtmlForm::new(".", Method::Get)
//!         .input(
//!             InputType::Text, "q", "Search", true, None,
//!             vec![], vec![Attr::Placeholder("enter query")])?
//!         .submit(None, "go!", vec![])?)
//! }
//!
//! // more elaborate example, with validation (both client- and
//! // server-side) and custom attributes
//! fn userform() -> Result<HtmlForm<'static>, FormError> {
//!     Ok(HtmlForm::new(".", Method::Post)
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
//!             ],
//!             vec![Attr::Title(
//!                 "Must contain 1 number and 1 non-word character")])?
//!         .input(
//!             InputType::Number, "age", "Age", true, None,
//!             vec![Cons::MinNumber(18.0)],
//!             vec![Attr::Step(1.0), Attr::Any("id", "age")])?
//!         .hidden(
//!             "csrf", Some("foo"), true,
//!             vec![Cons::Func(Box::new(|v| {
//!                 if v.as_string().as_str() != "foo" {
//!                     Err(ValidationError::new("invalid CSRF token"))
//!                 } else {
//!                     Ok(())
//!                 }
//!             }))],
//!             vec![])?
//!         .textarea("message", "Message", false, None, vec![])?
//!         .submit(None, "Submit", vec![])?
//!     )
//! }
//!
//! fn main() {
//!     let values = ValueMap::from_urlencoded(b"q=foo").unwrap();
//!     let form = searchform().unwrap().validate_and_set(values);
//!
//!     println!("{}", serde_json::to_string_pretty(&form).unwrap());
//!     assert_eq!(form.errors.len(), 0);
//!     assert_eq!(form.getone::<String>("q").unwrap(), "foo");
//!     assert_eq!(
//!         serde_json::to_string_pretty(&form).unwrap(),
//!         r#"{
//!   "action": ".",
//!   "method": "get",
//!   "errors": {},
//!   "fields": [
//!     {
//!       "name": "q",
//!       "label": "Search",
//!       "fieldtype": "input",
//!       "subtype": "text",
//!       "required": true,
//!       "multi": false,
//!       "choices": [],
//!       "attributes": {
//!         "placeholder": "enter query"
//!       },
//!       "value": "foo"
//!     },
//!     {
//!       "name": "",
//!       "label": "go!",
//!       "fieldtype": "input",
//!       "subtype": "submit",
//!       "required": false,
//!       "multi": false,
//!       "choices": [],
//!       "attributes": {},
//!       "value": ""
//!     }
//!   ]
//! }"#);
//!
//!     let values = ValueMap::from_urlencoded(
//!         b"username=johnny&name=Johnny&password=foobar-123&age=46&csrf=bar"
//!     ).unwrap();
//!     let form = userform().unwrap().validate_and_set(values);
//!
//!     assert_eq!(form.errors.len(), 1);
//!     assert_eq!(
//!         form.errors.get("csrf").unwrap(), "invalid CSRF token");
//!     assert_eq!(form.getone::<String>("username").unwrap(), "johnny");
//!     assert_eq!(form.getone::<String>("password").unwrap(), "foobar-123");
//!     assert_eq!(form.getone::<String>("csrf").unwrap(), "bar");
//! }
//! ```

mod error;
mod value;
mod types;
mod htmlform;

pub use crate::error::{FormError, ValidationError};
pub use crate::value::{ValueMap, Value, urldecode, UrlDecodingError};
pub use crate::types::{
    Method, Element, InputType, Select, Constraint, Attr};
pub use crate::htmlform::{HtmlForm, Field};
