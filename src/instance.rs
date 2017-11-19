use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use class::LoxClass;
use primitive::Value;

#[derive(Debug)]
struct Instance {
    class: LoxClass,
    fields: HashMap<String, Value>,
}

impl Instance {
    fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    fn get_field(&self, name: &str) -> Option<Value> {
        self.fields.get(name).cloned()
    }

    fn set_field(&mut self, field: String, value: Value) {
        self.fields.insert(field, value);
    }
}

#[derive(Clone, Debug)]
pub struct LoxInstance {
    inner: Rc<RefCell<Instance>>,
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self {
            inner: Rc::new(RefCell::new(Instance::new(class))),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let instance = self.inner.borrow();

        if let Some(value) = instance.get_field(name) {
            Some(value)
        } else if let Some(method) = instance.class.get_method(name, self.clone()) {
            Some(Value::Fun(method))
        } else {
            None
        }
    }

    pub fn set(&mut self, field: String, value: Value) {
        self.inner.borrow_mut().set_field(field, value);
    }

    pub fn class_name(&self) -> String {
        self.inner.borrow().class.name().into()
    }
}
