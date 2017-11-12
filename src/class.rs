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

    fn initializer(&self) -> Option<Rc<LoxFunction>> {
        self.methods.borrow().get("init").cloned()
    }
}

impl LoxCallable for LoxClass {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> ValueResult {
        let instance = LoxInstance::new(self.clone());

        if let Some(initializer) = self.initializer() {
            initializer
                .bind(instance.clone())
                .call(interpreter, arguments)
        } else {
            Ok(Value::Instance(instance))
        }
    }

    fn arity(&self) -> usize {
        self.initializer()
            .map(|initializer| initializer.arity())
            .unwrap_or(0)
    }

    fn name(&self) -> &str {
        &self.name
    }
}