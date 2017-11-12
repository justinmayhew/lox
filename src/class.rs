use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use callable::{LoxCallable, LoxFunction};
use instance::LoxInstance;
use interpreter::Interpreter;
use primitive::{Value, ValueResult};

type MethodMap = HashMap<String, Rc<LoxFunction>>;

#[derive(Clone, Debug)]
pub struct LoxClass {
    name: String,
    methods: Rc<RefCell<MethodMap>>,
}

impl LoxClass {
    pub fn new(name: String, methods: MethodMap) -> Self {
        Self {
            name,
            methods: Rc::new(RefCell::new(methods)),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn get_method(&self, name: &str, instance: LoxInstance) -> Option<Rc<LoxFunction>> {
        self.methods
            .borrow()
            .get(name)
            .cloned()
            .map(|method| Rc::new(method.bind(instance)))
    }
}

impl LoxCallable for LoxClass {
    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> ValueResult {
        let instance = LoxInstance::new(self.clone());
        Ok(Value::Instance(instance))
    }

    fn arity(&self) -> usize {
        0
    }

    fn name(&self) -> &str {
        &self.name
    }
}
