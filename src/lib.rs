//! Library to validate and generate HTML forms.
//!
//! ```rust
//! use es_htmlform::{HtmlForm, FormError, ValidationError};
//! use es_htmlform::value::ValueMap;
//! use es_htmlform::types::{InputType, Method, Constraint as Cons, Attr};
//!
//! // simple form with 1 field
//! fn searchform() -> Result<HtmlForm<'static>, FormError> {
//!     Ok(HtmlForm::new(".", Method::Get)
//!         .input(
//!             InputType::Text, "q", "Search", true,
//!             vec![], vec![Attr::Placeholder("enter query")])?
//!         .submit(None, "go!", vec![])?)
//! }
//!
//! // more elaborate example, with validation (both client- and
//! // server-side) and custom attributes
//! fn userform() -> Result<HtmlForm<'static>, FormError> {
//!     Ok(HtmlForm::new(".", Method::Post)
//!         .input(
//!             InputType::Text, "username", "Username", true,
//!             vec![
//!                 Cons::MinLength(5), Cons::MaxLength(16),
//!                 Cons::Pattern(r"^\w+$")],
//!             vec![])?
//!         .input(
//!             InputType::Text, "name", "Full name", true,
//!             vec![Cons::MinLength(0), Cons::MaxLength(64)], vec![])?
//!         .input(
//!             InputType::Password, "password", "Password", true,
//!             vec![
//!                 Cons::MinLength(6), Cons::MaxLength(64),
//!                 Cons::Pattern(r"(\d.*[^\w\s\d]|[^\w\s\d].*\d)"),
//!             ],
//!             vec![Attr::Title(
//!                 "Must contain 1 number and 1 non-word character")])?
//!         .input(
//!             InputType::Number, "age", "Age", true,
//!             vec![Cons::MinNumber(18.0)],
//!             vec![Attr::StepInt(1), Attr::Any("id", "age")])?
//!         .textarea("message", "Message", false, vec![], vec![])?
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
//!         .submit(None, "Submit", vec![])?
//!     )
//! }
//!
//! fn main() {
//!     let values = ValueMap::from_urlencoded(b"q=foo").unwrap();
//!     let mut form = searchform().unwrap();
//!     form.update(&values, true);
//!
//!     println!("{}", serde_json::to_string_pretty(&form).unwrap());
//!     assert_eq!(form.errors.len(), 0);
//!     assert_eq!(form.get_string("q").unwrap(), "foo");
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
//!       "element": "input",
//!       "type": "text",
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
//!       "element": "input",
//!       "type": "submit",
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
//!     let mut form = userform().unwrap();
//!     form.update(&values, true);
//!
//!     assert_eq!(form.errors.len(), 1);
//!     assert_eq!(
//!         form.errors.get("csrf").unwrap(), "invalid CSRF token");
//!     assert_eq!(form.get_string("username").unwrap(), "johnny");
//!     assert_eq!(form.get_string("password").unwrap(), "foobar-123");
//!     assert_eq!(form.get_string("csrf").unwrap(), "bar");
//! }
//! ```

mod error;
mod htmlform;

pub mod value;
pub mod types;
mod serde;

pub use crate::error::{FormError, ValidationError};
pub use crate::htmlform::{HtmlForm, Field};
