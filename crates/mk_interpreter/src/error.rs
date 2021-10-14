use std::{fmt::Display, rc::Rc};

use crate::{builtin::Builtin, object::Object};
use mk_parser::{span::WithSpan, token::Token};

// TODO: Add Spans to the entire codebase so errors can report a trace
#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    /// When attempting a prefix operation on an invalid type (e.g. !int)
    InvalidPrefixOperandType(WithSpan<Token>, Rc<Object>),
    /// When attempting an infix operation on invalid types/types that are not compatible (e.g. bool + bool, int + bool)
    InvalidInfixOperandType(WithSpan<Token>, Rc<Object>, Rc<Object>),
    // When attempting a logical infix (&& or ||) on invalid types/types that are not compatible (e.g. int && bool)
    // NOTE: Second one is Optional because we may have only evaluated the first when the error occurs
    InvalidLogicalInfixOperandType(WithSpan<Token>, Rc<Object>, Option<Rc<Object>>),
    /// When expecting a boolean conditional expression (e.g. if 1)
    ExpectedBooleanCondition(Rc<Object>),
    /// When referencing an identifier that does not exist/has not been defined
    IdentifierNotFound(String),
    /// When an object that is not a function is used with function call syntax
    NotAFunction(Rc<Object>),
    /// When a call's argument length does not match the expected function parameter length
    BadArity {
        expected: usize,
        got: usize,
    },
    /// When a call to builtin function passes an argument of an invalid/unsupported type
    InvalidArgumentType(Builtin, Rc<Object>),
    /// When attempting to index an object that does not support it (e.g. `1[0]`)
    IndexNotSupported(Rc<Object>),
    /// When attempting to index an object with a non-integer number (e.g. `[1, 2][true]`)
    InvalidIndexOperandType(Rc<Object>),
    /// When trying to get an element at a given index but it is outside of bounds
    IndexOutOfBounds {
        array: Rc<Object>,
        index: Rc<Object>,
    },
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RuntimeError::*;

        match self {
            InvalidPrefixOperandType(operator, right) => write!(
                f,
                "unsupported operand type for {} operator: `{}` ({}) {}",
                operator.value,
                right.typename(),
                right,
                // TODO: Somehow get the span of the right object/expression instead?
                operator.span.at_str()
            ),
            // TODO: Better error messages like "cannot add `bool` and `bool`"
            InvalidInfixOperandType(operator, left, right) => write!(
                f,
                "unsupported operand type(s) for {} operator: `{}` ({}) and `{}` ({}) {}",
                operator.value,
                left.typename(),
                left,
                right.typename(),
                right,
                // TODO: Somehow get the span of the objects/expressions instead?
                operator.span.at_str()
            ),
            InvalidLogicalInfixOperandType(operator, left, right) => {
                if let Some(right_obj) = right {
                    write!(
                        f,
                        "unsupported operand type(s) for logical {} operator: `{}` ({}) and `{}` ({}) {}", 
                        operator.value, 
                        left.typename(),
                        left,
                        right_obj.typename(), 
                        right_obj,
                        // TODO: Somehow get the span of the objects/expressions instead?
                        operator.span.at_str()
                    )
                }
                else {
                    write!(
                        f, 
                        "unsupported operand type for logical {} operator: `{}` ({}) {}",
                        operator.value, 
                        left.typename(),
                        left,
                        // TODO: Somehow get the span of the right object/expression instead?
                        operator.span.at_str()
                    )
                }
            }
            ExpectedBooleanCondition(expression) => write!(
                f,
                "expected a `boolean` condition but got `{}` ({})",
                expression.typename(),
                expression
            ),
            IdentifierNotFound(name) => write!(f, "identifier '{}' not found", name),
            NotAFunction(obj) => write!(f, "{} is not a function", obj),
            BadArity { expected, got } => {
                write!(f, "expected {} argument(s) but got {}.", expected, got)
            }
            InvalidArgumentType(builtin, obj) => write!(
                f,
                "unsupported argument type for {} function: `{}` ({})",
                builtin.name(),
                obj.typename(),
                obj
            ),
            IndexNotSupported(left) => {
                write!(
                    f,
                    "index operator not supported for `{}` ({})",
                    left.typename(),
                    left
                )
            }
            InvalidIndexOperandType(index) => {
                write!(
                    f,
                    "unsupported index operand type: `{}` ({})",
                    index.typename(),
                    index
                )
            }
            IndexOutOfBounds { array, index } => {
                write!(f, "index {} out of bounds for array {}", index, array)
            }
        }
    }
}
