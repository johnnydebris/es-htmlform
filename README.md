# ES HtmlForm

HtmlForm is a library to build, validate and render HTML(5) forms. It aims to
follow the HTML specifications as closely as possible, and provide a complete
solution to easily build valid forms and validate data both on the server
and the client.

## Example

```rust
    use htmlform::{HtmlForm, ValueMap, Method, InputType, Constraint, Attr};

    fn main() {
        let form = HtmlForm::new(".", Method::Get)
            .input(
                InputType::Text, "q", "Search", true, None,
                vec![Constraint::MinLength(2)],
                vec![Attr::Placeholder("enter value...")]
                ).unwrap()
            .submit(None, "go!", vec![]).unwrap();

        let values = ValueMap::from_urlencoded(b"q=foo").unwrap();
        form.validate_and_set(values);

        assert_eq!(form.errors.len(), 0);
        assert_eq!(form.getone::<String>("q").unwrap(), "foo");

        println!("{}", serde_json::to_string_pretty(&form));
    }
```

## Features

* Follows a builder-style pattern to safely build correct form structures.

* Provides a complete (XXX not yet!) set of enums for all HTML elements and
  attributes, with HTML validity checks on building the form.

* Provides client- and server-side validation based on HTML constraint
  attributes (e.g. `max`/`min`, `maxlength`/`minlength`, `pattern`).

* Allows per-field custom server-side validation.

* Parses `application/x-www-form-urlencoded` to a data container (`ValueMap`)
  to use as values for the form - support for `multipart/form-data` is
  planned.

* Serializes to JSON (and other formats) by implementing
  [Serde](https://docs.rs/serde/)'s `Serialize` trait.

*HTML generation functionality is not directly provided*, as users will always
want to render their HTML in a specific manner. Instead, `HtmlForm`
implements [Serde](https://docs.rs/serde/)'s `Serialize`
trait so it can easily be converted to JSON for client-side rendering or to
use as datastructure for templating languages like
[handlebars](https://docs.rs/handlebars/).

Note that this library is in a very early stage, it's not complete yet
(some attributes and constraints are missing, as is file upload/multipart
support), the code needs cleanup/repartitioning and there are some things I
would like to add in the near future (examples of usage to render client-side
or server-side forms, integration of some i18n library (fluent?), integration
code for [Actix](https://docs.rs/actix/), [hyper](https://docs.rs/hyper/),
etc.).

Also note that I am relatively new to Rust, and I am very open to suggestions
for improvement!

For suggestions, questions, remarks or whatever, feel free to email me at
[johnnydebris@gmail.com](mailto::johnnydebris@gmail.com).
