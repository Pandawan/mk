use std::{fmt::Display, rc::Rc};

use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Nil,
    /// Special object to encapsulate a return-ed value while it goes up scopes.
    /// This is never seen by the user.
    ReturnValue(Rc<Object>),
    // TODO: Add Spans to the entire codebase so errors can report a trace
    Error(RuntimeError),
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

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    /// When attempting a prefix operation on an invalid type (e.g. !int)
    InvalidPrefixOperandType(Token, Rc<Object>),
    /// When attempting an infix operation on invalid types/types that are not compatible (e.g. bool + bool, int + bool)
    InvalidInfixOperandType(Token, Rc<Object>, Rc<Object>),
    /// When expecting a boolean conditional expression (e.g. if 1)
    ExpectedBooleanCondition(Rc<Object>),
    /// When referencing an identifier that does not exist/has not been defined
    IdentifierNotFound(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidPrefixOperandType(operator, right) => write!(
                f,
                "unsupported operand type for {} operator: `{}` ({})",
                operator,
                right.typename(),
                right,
            ),
            // TODO: Better error messages like "cannot add `bool` and `bool`"
            Self::InvalidInfixOperandType(operator, left, right) => write!(
                f,
                "unsupported operand type(s) for {} operator: `{}` ({}) and `{}` ({})",
                operator,
                left.typename(),
                left,
                right.typename(),
                right
            ),
            Self::ExpectedBooleanCondition(expression) => write!(
                f,
                "expected a `boolean` condition but got `{}` ({})",
                expression.typename(),
                expression
            ),
            Self::IdentifierNotFound(name) => write!(f, "identifier '{}' not found", name),
        }
    }
}
