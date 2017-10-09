use time;

use interpreter::Interpreter;
use primitive::{Value, ValueResult};

pub trait LoxCallable {
    fn call(&self, &mut Interpreter, Vec<Value>) -> ValueResult;
    fn arity(&self) -> usize;
    fn name(&self) -> &'static str;
}

pub struct Clock;

impl LoxCallable for Clock {
    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> ValueResult {
        Ok(Value::Int(time::now().to_timespec().sec))
    }

    fn arity(&self) -> usize {
        0
    }

    fn name(&self) -> &'static str {
        "clock"
    }
}
