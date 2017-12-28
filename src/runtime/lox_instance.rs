use std::cell::RefCell;
use std::collections::HashMap;

use super::{LoxClass, Value};

#[derive(Debug)]
pub struct LoxInstance {
    class: LoxClass,
    fields: RefCell<HashMap<String, Value>>,
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: RefCell::new(HashMap::new()),
        }
    }

    pub fn get_field(&self, name: &str) -> Option<Value> {
        self.fields.borrow().get(name).cloned()
    }

    pub fn set(&self, field: String, value: Value) {
        self.fields.borrow_mut().insert(field, value);
    }

    pub fn class(&self) -> &LoxClass {
        &self.class
    }
}
