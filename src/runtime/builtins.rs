use time;

use super::{Interpreter, LoxCallable, Result, Value};

#[derive(Debug)]
pub struct Clock;

impl LoxCallable for Clock {
    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> Result<Value> {
        Ok(Value::Number(time::now().to_timespec().sec as f64))
    }

    fn arity(&self) -> usize {
        0
    }

    fn name(&self) -> &str {
        "clock"
    }
}
