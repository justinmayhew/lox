use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use primitive::Value;

#[derive(Clone, Debug)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            enclosing: None,
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
                Some(ref mut env) => env.borrow_mut().assign(key, value),
                None => false,
            }
        }
    }

    pub fn assign_at(&mut self, key: &str, value: Value, hops: usize) {
        if hops == 0 {
            assert!(self.values.contains_key(key));
            self.values.insert(key.into(), value);
        } else if let Some(ref env) = self.enclosing {
            env.borrow_mut().assign_at(key, value, hops - 1)
        } else {
            panic!("not enough environments to hop to");
        }
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.values
            .get(key)
            .cloned()
            .or_else(|| match self.enclosing {
                Some(ref env) => env.borrow().get(key),
                None => None,
            })
    }

    pub fn get_at(&self, key: &str, hops: usize) -> Value {
        if hops == 0 {
            self.get(key).expect("variable not defined")
        } else if let Some(ref env) = self.enclosing {
            env.borrow().get_at(key, hops - 1)
        } else {
            panic!("not enough environments to hop to");
        }
    }

    pub fn set_enclosing(&mut self, enclosing: Rc<RefCell<Environment>>) {
        self.enclosing = Some(enclosing);
    }

    pub fn pop_enclosing(&mut self) -> Rc<RefCell<Environment>> {
        self.enclosing
            .take()
            .expect("pop_enclosing with no enclosing environment")
    }
}
