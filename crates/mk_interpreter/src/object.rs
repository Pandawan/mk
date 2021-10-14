use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{builtin::Builtin, environment::Environment};

use mk_parser::ast::{BlockExpression, IdentifierLiteral};

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
        }
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
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Array {
    pub elements: Vec<Rc<Object>>,
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements: Vec<String> = (&self.elements).iter().map(|e| e.to_string()).collect();
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
        let params: Vec<String> = (&self.parameters).iter().map(|p| p.to_string()).collect();

        write!(f, "fn ({}) {{\n{}\n}} ", params.join(", "), self.body)
    }
}

impl PartialEq for Function {
    fn eq(&self, _: &Function) -> bool {
        // This should never be used?
        panic!("PartialEq is not implemented for `function`");
    }
}
