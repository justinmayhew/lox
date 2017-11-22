use std::fmt;
use std::rc::Rc;

use time;

use environment::Environment;
use instance::LoxInstance;
use interpreter::Interpreter;
use parser::Function;
use primitive::{Error, Value, ValueResult};

pub trait LoxCallable: fmt::Display + fmt::Debug {
    fn call(&self, &mut Interpreter, Vec<Value>) -> ValueResult;
    fn arity(&self) -> usize;
    fn name(&self) -> &str;
}

#[derive(Debug)]
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

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn clock>")
    }
}

#[derive(Clone, Debug)]
pub struct LoxFunction {
    function: Function,
    closure: Environment,
    is_initializer: bool,
}

impl LoxFunction {
    pub fn new(function: Function, closure: Environment, is_initializer: bool) -> Self {
        Self {
            function,
            closure,
            is_initializer,
        }
    }

    pub fn bind(&self, instance: Rc<LoxInstance>) -> Self {
        let env = Environment::with_enclosing(self.closure.clone());
        env.define("this".into(), Value::Instance(instance));
        Self {
            function: self.function.clone(),
            closure: env,
            is_initializer: self.is_initializer,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> ValueResult {
        let env = Environment::with_enclosing(self.closure.clone());

        // Bind the parameters to the arguments the function was called with.
        for (key, value) in self.function.parameters.iter().zip(arguments.iter()) {
            env.define(key.clone(), value.clone());
        }

        let result = interpreter.execute_block(&self.function.body, env.clone());

        if self.is_initializer && result.is_ok() {
            return Ok(env.get_at("this", 1));
        }

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

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.function.name)
    }
}
