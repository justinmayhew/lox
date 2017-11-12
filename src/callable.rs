use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

use time;

use environment::Environment;
use instance::LoxInstance;
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

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct LoxFunction {
    function: Function,
    closure: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    pub fn new(function: Function, closure: Rc<RefCell<Environment>>) -> Self {
        Self { function, closure }
    }

    pub fn bind(&self, instance: LoxInstance) -> Self {
        let mut env = Environment::new();
        env.set_enclosing(Rc::clone(&self.closure));
        env.define("this".into(), Value::Instance(instance));
        Self {
            function: self.function.clone(),
            closure: Rc::new(RefCell::new(env)),
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
        for (key, value) in self.function.parameters.iter().zip(arguments.iter()) {
            interpreter
                .env
                .borrow_mut()
                .define(key.clone(), value.clone());
        }

        let mut result = Ok(());

        for stmt in &self.function.body {
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
        self.function.parameters.len()
    }

    fn name(&self) -> &str {
        &self.function.name
    }
}
