use std::rc::Rc;
use std::result;

use callable::LoxCallable;

#[derive(Clone)]
pub enum Value {
    Str(String),
    Int(i64),
    Bool(bool),
    Nil,
    Fun(Rc<LoxCallable>),
}

#[derive(Debug)]
pub enum Error {
    TypeError(String),
    DivideByZero,
    UndefinedVar(String),
    ArityError(String),
}

pub type Result<T> = result::Result<T, Error>;
pub type ValueResult = Result<Value>;

impl Value {
    pub fn unwrap_callable(&self) -> Rc<LoxCallable> {
        if let Value::Fun(ref f) = *self {
            Rc::clone(f)
        } else {
            panic!("unwrap_callable on {:?}", self);
        }
    }
}
