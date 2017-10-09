use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

use time;

use environment::Environment;
use interpreter::Interpreter;
use parser::Stmt;
use primitive::{Error, Value, ValueResult};

pub trait LoxCallable {
    fn call(&self, &mut Interpreter, Vec<Value>) -> ValueResult;
    fn arity(&self) -> usize;
    fn name(&self) -> &str;
}

pub struct Clock;

impl LoxCallable for Clock {
    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> ValueResult {
        Ok(Value::Int(time::now().to_timespec().sec))
    }

    fn arity(&self) -> usize {
        0
    }

    fn name(&self) -> &str {
        "clock"
    }
}

pub struct LoxFunction {
    name: String,
    parameters: Vec<String>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    pub fn new(
        name: String,
        parameters: Vec<String>,
        body: Vec<Stmt>,
        closure: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            closure,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> ValueResult {
        // Set up new environment for this block.
        let previous = mem::replace(
            &mut interpreter.env,
            Rc::new(RefCell::new(Environment::new())),
        );
        interpreter
            .env
            .borrow_mut()
            .set_enclosing(Rc::clone(&self.closure));

        // Bind the parameters to the arguments the function was called with.
        for (key, value) in self.parameters.iter().zip(arguments.iter()) {
            interpreter
                .env
                .borrow_mut()
                .define(key.clone(), value.clone());
        }

        let mut result = Ok(());

        for stmt in &self.body {
            result = interpreter.exec_stmt(stmt);
            if result.is_err() {
                break;
            }
        }

        // Restore previous environment.
        interpreter.env = previous;

        match result {
            Ok(()) => Ok(Value::Nil),
            Err(Error::Return(value)) => Ok(value),
            Err(e) => Err(e),
        }
    }

    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn name(&self) -> &str {
        &self.name
    }
}
