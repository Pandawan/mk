use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::error::RuntimeError;
use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Environment {
    // TODO: Add some way to identify environments (e.g. global, comes from a specific fn, etc.) for debugging & tracing
    store: HashMap<String, Rc<Object>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    /// Create a new environment that is enclosed by a given outer environment
    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        match self.store.get(name) {
            Some(obj) => Some(Rc::clone(obj)),
            // If not found in this environment, look for it in the outer environment
            None => match self.outer {
                Some(ref outer) => outer.borrow_mut().get(name),
                None => None,
            },
        }
    }

    pub fn define(&mut self, name: String, value: Rc<Object>) {
        self.store.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: Rc<Object>) -> Result<(), RuntimeError> {
        if self.store.contains_key(&name) {
            self.store.insert(name, value);
            Ok(())
        } else {
            match self.outer {
                Some(ref outer) => outer.borrow_mut().assign(name, value),
                None => Err(RuntimeError::IdentifierNotFound(name)),
            }
        }
    }

    pub fn depth(&self) -> usize {
        match &self.outer {
            // Recursively add the depth
            Some(parent_env) => 1 + parent_env.borrow().depth(),
            None => 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::environment::Environment;

    #[test]
    pub fn test_depth() {
        let env1 = Rc::new(RefCell::new(Environment::new()));
        assert_eq!(env1.borrow().depth(), 1);

        let env2 = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(&env1))));
        assert_eq!(env1.borrow().depth(), 1);
        assert_eq!(env2.borrow().depth(), 2);

        let env3 = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(&env2))));
        assert_eq!(env2.borrow().depth(), 2);
        assert_eq!(env3.borrow().depth(), 3);
    }
}
