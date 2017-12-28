use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::rc::Rc;

use super::Value;

#[derive(Debug)]
struct Inner {
    enclosing: Option<Environment>,
    values: HashMap<String, Value>,
}

impl Inner {
    fn new() -> Self {
        Self {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    fn with_enclosing(enclosing: Environment) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    pub fn ancestor(&self, hops: usize) -> Environment {
        self.enclosing
            .clone()
            .expect("missing enclosing environment")
            .ancestor(hops)
    }

    fn define(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    fn assign(&mut self, key: String, value: Value) -> bool {
        match self.values.entry(key) {
            Entry::Occupied(mut entry) => {
                entry.insert(value);
                true
            }
            Entry::Vacant(_) => false,
        }
    }

    fn get(&self, key: &str) -> Option<Value> {
        self.values.get(key).cloned()
    }
}

#[derive(Clone, Debug)]
pub struct Environment {
    inner: Rc<RefCell<Inner>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            inner: Rc::new(RefCell::new(Inner::new())),
        }
    }
}

impl Environment {
    pub fn with_enclosing(enclosing: Environment) -> Self {
        Self {
            inner: Rc::new(RefCell::new(Inner::with_enclosing(enclosing))),
        }
    }

    pub fn ancestor(&self, hops: usize) -> Environment {
        if hops == 0 {
            Environment {
                inner: Rc::clone(&self.inner),
            }
        } else {
            self.inner.borrow().ancestor(hops - 1)
        }
    }

    pub fn define(&self, key: String, value: Value) {
        self.inner.borrow_mut().define(key, value);
    }

    pub fn assign(&self, key: String, value: Value) -> bool {
        self.inner.borrow_mut().assign(key, value)
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.inner.borrow().get(key)
    }
}
