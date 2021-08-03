use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Nil,
}

impl Object {
    fn typename(&self) -> String {
        match self {
            Self::Integer(_) => "integer".into(),
            Self::Float(_) => "float".into(),
            Self::Boolean(_) => "boolean".into(),
            Self::Nil => "nil".into(),
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
        }
    }
}
