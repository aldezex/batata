use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn get(&self, name: &str) -> (Option<&Object>, bool) {
        match self.store.get(name) {
            Some(object) => (Some(object), true),
            None => match &self.outer {
                Some(outer) => outer.get(name),
                None => (None, false),
            },
        }
    }

    pub fn set(&mut self, name: &str, value: Object) -> Object {
        self.store.insert(name.to_string(), value.clone());
        value
    }

    pub fn new_enclosed_environment(&self, outer: Environment) -> Environment {
        let mut env = Environment::new();
        env.outer = Some(Box::new(outer));
        env
    }
}
