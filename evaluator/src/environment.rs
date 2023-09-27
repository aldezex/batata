use std::collections::HashMap;

use crate::object::Object;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> (Option<&Object>, bool) {
        match self.store.get(name) {
            Some(o) => (Some(o), true),
            None => (None, false),
        }
    }

    pub fn set(&mut self, name: &str, value: Object) -> Object {
        self.store.insert(name.to_string(), value.clone());
        value
    }
}
