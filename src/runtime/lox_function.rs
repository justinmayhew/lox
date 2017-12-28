use std::rc::Rc;

use parser::{Function, Identifier, Stmt};
use super::{Environment, Error, Interpreter, LoxCallable, LoxInstance, Result, Value};

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
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value> {
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
