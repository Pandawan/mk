use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Nil,
    /// Special object to encapsulate a return-ed value while it goes up scopes.
    /// This is never seen by the user.
    ReturnValue(Box<Object>),
    // TODO: Add Spans to the entire codebase so errors can report a trace
    // TODO: Use a preset-error enum to standardize error messages all in one place
    Error(String),
}

impl Object {
    pub fn typename(&self) -> String {
        match self {
            Self::Integer(_) => "integer".into(),
            Self::Float(_) => "float".into(),
            Self::Boolean(_) => "boolean".into(),
            Self::Nil => "nil".into(),
            Self::ReturnValue(obj) => obj.typename(),
            Self::Error(_) => "error".into(),
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            Self::Error(_) => true,
            _ => false,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(value) => write!(f, "{}", value),
            Self::Float(value) => write!(f, "{}", ryu::Buffer::new().format(*value)),
            Self::Boolean(value) => write!(f, "{}", value),
            Self::Nil => write!(f, "nil"),
            Self::ReturnValue(obj) => write!(f, "{}", obj),
            Self::Error(message) => write!(f, "Error: {}", message),
        }
    }
}
