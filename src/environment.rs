use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use primitive::Value;

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

    fn define(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    /// Returns `true` if the variable previously been defined.
    fn assign(&mut self, key: &str, value: Value) -> bool {
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

    fn assign_at(&mut self, key: &str, value: Value, hops: usize) {
        if hops == 0 {
            assert!(self.values.contains_key(key));
            self.values.insert(key.into(), value);
        } else if let Some(ref mut env) = self.enclosing {
            env.assign_at(key, value, hops - 1)
        } else {
            panic!("not enough environments to hop to");
        }
    }

    fn get(&self, key: &str) -> Option<Value> {
        self.values
            .get(key)
            .cloned()
            .or_else(|| match self.enclosing {
                Some(ref env) => env.get(key),
                None => None,
            })
    }

    fn get_at(&self, key: &str, hops: usize) -> Value {
        if hops == 0 {
            self.get(key).expect("variable not defined")
        } else if let Some(ref env) = self.enclosing {
            env.get_at(key, hops - 1)
        } else {
            panic!("not enough environments to hop to");
        }
    }
}

#[derive(Clone, Debug)]
pub struct Environment {
    inner: Rc<RefCell<Inner>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(Inner::new())),
        }
    }

    pub fn with_enclosing(enclosing: Environment) -> Self {
        Self {
            inner: Rc::new(RefCell::new(Inner::with_enclosing(enclosing))),
        }
    }

    pub fn define(&mut self, key: String, value: Value) {
        self.inner.borrow_mut().define(key, value);
    }

    pub fn assign(&mut self, key: &str, value: Value) -> bool {
        self.inner.borrow_mut().assign(key, value)
    }

    pub fn assign_at(&mut self, key: &str, value: Value, hops: usize) {
        self.inner.borrow_mut().assign_at(key, value, hops);
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.inner.borrow().get(key)
    }

    pub fn get_at(&self, key: &str, hops: usize) -> Value {
        self.inner.borrow().get_at(key, hops)
    }
}
