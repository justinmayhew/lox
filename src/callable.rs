use std::fmt;
use std::rc::Rc;

use time;

use environment::Environment;
use instance::LoxInstance;
use interpreter::Interpreter;
use parser::{FunctionDecl, FunctionExpr, Identifier, Stmt};
use primitive::{Error, Value, ValueResult};

#[derive(Clone, Debug)]
pub enum Function {
    Decl(FunctionDecl),
    Expr(FunctionExpr),
}

pub trait LoxCallable: fmt::Display + fmt::Debug {
    fn call(&self, &mut Interpreter, Vec<Value>) -> ValueResult;
    fn arity(&self) -> usize;
    fn name(&self) -> &str;
}

#[derive(Debug)]
pub struct Clock;

impl LoxCallable for Clock {
    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> ValueResult {
        Ok(Value::Number(time::now().to_timespec().sec as f64))
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

    fn parameters(&self) -> &[Identifier] {
        match self.function {
            Function::Decl(ref f) => &f.parameters,
            Function::Expr(ref f) => &f.parameters,
        }
    }

    fn body(&self) -> &[Stmt] {
        match self.function {
            Function::Decl(ref f) => &f.body,
            Function::Expr(ref f) => &f.body,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> ValueResult {
        let env = Environment::with_enclosing(self.closure.clone());

        // Bind the parameters to the arguments the function was called with.
        for (parameter, value) in self.parameters().iter().zip(arguments.iter()) {
            env.define(parameter.name.clone(), value.clone());
        }

        let result = interpreter.execute_block(self.body(), env.clone());

        if self.is_initializer && result.is_ok() {
            return Ok(env.ancestor(1).get("this").unwrap());
        }

        match result {
            Ok(()) => Ok(Value::Nil),
            Err(Error::Return(value)) => Ok(value),
            Err(e) => Err(e),
        }
    }

    fn arity(&self) -> usize {
        match self.function {
            Function::Decl(ref f) => f.parameters.len(),
            Function::Expr(ref f) => f.parameters.len(),
        }
    }

    fn name(&self) -> &str {
        match self.function {
            Function::Decl(ref f) => f.name(),
            Function::Expr(_) => "anonymous",
        }
    }
}

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.name())
    }
}
