use std::io;

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

#[derive(Debug)]
pub struct Input;

impl LoxCallable for Input {
    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> Result<Value> {
        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .expect("input: error reading line");

        if let Some(c) = line.pop() {
            if c != '\n' {
                line.push(c);
            }
        }

        Ok(Value::String(line))
    }

    fn arity(&self) -> usize {
        0
    }

    fn name(&self) -> &str {
        "input"
    }
}
