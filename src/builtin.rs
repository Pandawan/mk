use std::{fmt::Display, rc::Rc};

use crate::object::{Object, RuntimeError};

#[derive(Debug, PartialEq)]
pub enum Builtin {
    Len,
    Type,
    // TODO: Print
    // TODO: Time
}

impl Builtin {
    pub fn lookup(name: &str) -> Option<Builtin> {
        match name {
            "len" => Some(Builtin::Len),
            "type" => Some(Builtin::Type),
            _ => None,
        }
    }

    pub fn name(&self) -> String {
        match self {
            Builtin::Len => "len".into(),
            Builtin::Type => "type".into(),
        }
    }

    pub fn apply(&self, args: Vec<Rc<Object>>) -> Result<Rc<Object>, RuntimeError> {
        match self {
            Builtin::Len => {
                if args.len() != 1 {
                    return Err(RuntimeError::BadArity {
                        expected: 1,
                        got: args.len(),
                    });
                }

                let arg = Rc::clone(args.first().unwrap());
                match arg.as_ref() {
                    Object::String(str) => Ok(Rc::new(Object::Integer(str.len() as i64))),
                    Object::Array(arr) => Ok(Rc::new(Object::Integer(arr.elements.len() as i64))),
                    _ => Err(RuntimeError::InvalidArgumentType(Builtin::Len, arg)),
                }
            }
            Builtin::Type => {
                if args.len() != 1 {
                    return Err(RuntimeError::BadArity {
                        expected: 1,
                        got: args.len(),
                    });
                }
                let arg = Rc::clone(args.first().unwrap());
                Ok(Rc::new(Object::String(arg.typename())))
            }
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "builtin function {}", self.name())
    }
}
