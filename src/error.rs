use std::fmt;
use std::error::Error;

/// Returned on form definition errors.
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

/// Returned on form validation errors.
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
