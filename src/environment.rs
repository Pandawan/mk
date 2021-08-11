use std::{collections::HashMap, rc::Rc};

use crate::object::Object;

#[derive(Clone)]
pub struct Environment {
    // TODO: Use Rc<Object> everywhere instead?
    store: HashMap<String, Rc<Object>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        match self.store.get(name) {
            Some(obj) => Some(Rc::clone(obj)),
            None => None,
        }
    }

    pub fn set(&mut self, name: String, value: Rc<Object>) {
        self.store.insert(name, value);
    }
}
