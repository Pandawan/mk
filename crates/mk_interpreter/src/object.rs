use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{
    builtin::Builtin,
    environment::Environment,
};

use mk_parser::{ast::{BlockExpression, IdentifierLiteral}, token::Token };

// TODO: Add Range object (to allow for myArr[0..1] AND for i in 0..1)
#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Nil,
    Array(Array),
    Function(Function),
    Builtin(Builtin),
    /// Special object to encapsulate a return-ed value while it goes up scopes.
    /// This is never seen by the user.
    ReturnValue(Rc<Object>),
    // TODO: Not sure if Errors should be objects? It may be better to represent them through Result::Err and have "catchable" errors be actual error objects
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
            String(_) => "string".into(),
            Nil => "nil".into(),
            Array(_) => "array".into(),
            Function(_) => "function".into(),
            Builtin(_) => "builtin".into(),
            ReturnValue(obj) => obj.typename(),
            Error(_) => "error".into(),
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }

    /// Converts the given value to a string (in the format of a code object).
    ///
    /// Use this anywhere a programmer expects to see the code-version of an object (e.g. in the REPL).
    /// # Examples
    /// ```rust
    /// use mk_interpreter::object::Object;
    /// 
    /// let obj = Object::String("hello world".to_string());
    ///
    /// assert_eq!(obj.to_code_string(), "\"hello world\"");
    /// ```
    pub fn to_code_string(&self) -> String {
        use Object::*;

        match self {
            String(value) => format!("\"{}\"", value),
            value => value.to_string(),
        }
    }
}

impl Display for Object {
    /// toString() form at runtime
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Object::*;

        match self {
            Integer(value) => write!(f, "{}", value),
            Float(value) => write!(f, "{}", ryu::Buffer::new().format(*value)),
            Boolean(value) => write!(f, "{}", value),
            String(value) => write!(f, "{}", value),
            Nil => write!(f, "nil"),
            Array(array) => write!(f, "{}", array),
            Function(func) => write!(f, "{}", func),
            Builtin(builtin) => write!(f, "{}", builtin),
            ReturnValue(obj) => write!(f, "{}", obj),
            Error(message) => write!(f, "Error: {}", message),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Array {
    pub elements: Vec<Rc<Object>>,
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements: Vec<String> = (&self.elements)
            .iter()
            .map(|e| e.to_string())
            .collect();
        write!(f, "[{}]", elements.join(", "))
    }
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<IdentifierLiteral>,
    pub body: Rc<BlockExpression>,
    pub env: Rc<RefCell<Environment>>,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<String> = (&self.parameters)
            .iter()
            .map(|p| p.to_string())
            .collect();

        write!(f, "fn ({}) {{\n{}\n}} ", params.join(", "), self.body)
    }
}

impl PartialEq for Function {
    fn eq(&self, _: &Function) -> bool {
        // This should never be used?
        panic!("PartialEq is not implemented for `function`");
    }
}

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    /// When attempting a prefix operation on an invalid type (e.g. !int)
    InvalidPrefixOperandType(Token, Rc<Object>),
    /// When attempting an infix operation on invalid types/types that are not compatible (e.g. bool + bool, int + bool)
    InvalidInfixOperandType(Token, Rc<Object>, Rc<Object>),
    // When attempting a logical infix (&& or ||) on invalid types/types that are not compatible (e.g. int && bool)
    // NOTE: Second one is Optional because we may have only evaluated the first when the error occurs
    InvalidLogicalInfixOperandType(Token, Rc<Object>, Option<Rc<Object>>),
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
            InvalidLogicalInfixOperandType(operator, left, right) => {
                if let Some(right_obj) = right {
                    write!(
                        f,
                        "unsupported operand type(s) for logical {} operator: `{}` ({}) and `{}` ({})", 
                        operator, 
                        left.typename(),
                        left,
                        right_obj.typename(), 
                        right_obj
                    )
                }
                else {
                    write!(
                        f, 
                        "unsupported operand type for logical {} operator: `{}` ({})",
                        operator, 
                        left.typename(),
                        left
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
