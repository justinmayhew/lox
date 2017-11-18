use std::fmt;
use std::rc::Rc;
use std::result;

use callable::LoxCallable;
use class::LoxClass;
use instance::LoxInstance;

#[derive(Clone)]
pub enum Value {
    Str(String),
    Int(i64),
    Bool(bool),
    Nil,
    Fun(Rc<LoxCallable>),
    Class(LoxClass),
    Instance(LoxInstance),
}

#[derive(Debug)]
pub enum Error {
    Return(Value),
    RuntimeError(String),
    TypeError(String),
    DivideByZero,
    UndefinedVar(String),
    ArityError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Return(ref value) => write!(f, "Return {}", value),
            Error::RuntimeError(ref msg) => write!(f, "RuntimeError {}", msg),
            Error::TypeError(ref msg) => write!(f, "TypeError {}", msg),
            Error::DivideByZero => write!(f, "DivideByZero: division by zero"),
            Error::UndefinedVar(ref name) => {
                write!(f, "UndefinedVar: name '{}' is not defined", name)
            }
            Error::ArityError(ref msg) => write!(f, "ArityError: {}", msg),
        }
    }
}

pub type Result<T> = result::Result<T, Error>;
pub type ValueResult = Result<Value>;

impl Value {
    pub fn unwrap_callable(&self) -> Rc<LoxCallable> {
        match *self {
            Value::Fun(ref f) => Rc::clone(f),
            Value::Class(ref c) => Rc::new(c.clone()),
            ref value => panic!("{:?} is not callable", value),
        }
    }
}
