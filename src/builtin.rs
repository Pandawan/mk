use std::{fmt::Display, rc::Rc};

use crate::object::{Object, RuntimeError};

#[derive(Debug, PartialEq)]
pub enum Builtin {
    Len,
    // TODO: Type
    // TODO: Print
    // TODO: Time
}

impl Builtin {
    pub fn lookup(name: &str) -> Option<Builtin> {
        match name {
            "len" => Some(Builtin::Len),
            _ => None,
        }
    }

    pub fn name(&self) -> String {
        match self {
            Len => "len".into(),
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
                    // TODO: Array length
                    _ => Err(RuntimeError::InvalidArgumentType(Builtin::Len, arg)),
                }
            }
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "builtin function {}", self.name())
    }
}
