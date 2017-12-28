use std::fmt;

use super::{Interpreter, Result, Value};

pub trait LoxCallable: fmt::Debug {
    fn call(&self, &mut Interpreter, Vec<Value>) -> Result<Value>;
    fn arity(&self) -> usize;
    fn name(&self) -> &str;
}

impl fmt::Display for LoxCallable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.name())
    }
}
