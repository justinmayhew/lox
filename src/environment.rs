use std::collections::HashMap;

use parser::Value;

pub struct Environment {
    enclosing: Option<Box<Environment>>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new(enclosing: Option<Box<Environment>>) -> Self {
        Environment {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    /// Returns `true` if the variable previously been defined.
    pub fn assign(&mut self, key: &str, value: Value) -> bool {
        if self.values.contains_key(key) {
            self.values.insert(key.into(), value);
            true
        } else {
            match self.enclosing {
                Some(ref mut env) => env.assign(key, value),
                None => false,
            }
        }
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.values.get(key).or_else(|| match self.enclosing {
            Some(ref env) => env.get(key),
            None => None,
        })
    }

    pub fn set_enclosing(&mut self, enclosing: Environment) {
        self.enclosing = Some(Box::new(enclosing));
    }

    pub fn pop_enclosing(&mut self) -> Self {
        *self.enclosing
            .take()
            .expect("pop_enclosing with no enclosing environmnet")
    }
}
