use time;

use environment::Environment;
use interpreter::Interpreter;
use parser::Stmt;
use primitive::{Value, ValueResult};
use std::mem;

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
}

impl LoxFunction {
    pub fn new(name: String, parameters: Vec<String>, body: Vec<Stmt>) -> Self {
        Self {
            name,
            parameters,
            body,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> ValueResult {
        // Set up new environment for this block.
        let previous = mem::replace(&mut interpreter.env, Environment::new(None));
        interpreter.env.set_enclosing(previous);

        // Bind the parameters to the arguments the function was called with.
        for (key, value) in self.parameters.iter().zip(arguments.iter()) {
            interpreter.env.define(key.clone(), value.clone());
        }

        let mut result = Ok(());

        for stmt in &self.body {
            result = interpreter.exec_stmt(stmt);
            if result.is_err() {
                break;
            }
        }

        // Restore previous environment.
        let mut env = mem::replace(&mut interpreter.env, Environment::new(None));
        let previous = env.pop_enclosing();
        interpreter.env = previous;

        result.map(|_| Value::Nil)
    }

    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn name(&self) -> &str {
        &self.name
    }
}
