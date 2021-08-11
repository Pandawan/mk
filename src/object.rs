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
        use Object::*;

        match self {
            Integer(_) => "integer".into(),
            Float(_) => "float".into(),
            Boolean(_) => "boolean".into(),
            Nil => "nil".into(),
            ReturnValue(obj) => obj.typename(),
            Error(_) => "error".into(),
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
        use Object::*;

        match self {
            Integer(value) => write!(f, "{}", value),
            Float(value) => write!(f, "{}", ryu::Buffer::new().format(*value)),
            Boolean(value) => write!(f, "{}", value),
            Nil => write!(f, "nil"),
            ReturnValue(obj) => write!(f, "{}", obj),
            Error(message) => write!(f, "Error: {}", message),
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
        use RuntimeError::*;

        match self {
            InvalidPrefixOperandType(operator, right) => write!(
                f,
                "unsupported operand type for {} operator: `{}` ({})",
                operator,
                right.typename(),
                right,
            ),
            // TODO: Better error messages like "cannot add `bool` and `bool`"
            InvalidInfixOperandType(operator, left, right) => write!(
                f,
                "unsupported operand type(s) for {} operator: `{}` ({}) and `{}` ({})",
                operator,
                left.typename(),
                left,
                right.typename(),
                right
            ),
            ExpectedBooleanCondition(expression) => write!(
                f,
                "expected a `boolean` condition but got `{}` ({})",
                expression.typename(),
                expression
            ),
            IdentifierNotFound(name) => write!(f, "identifier '{}' not found", name),
        }
    }
}
