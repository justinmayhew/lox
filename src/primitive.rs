use std::fmt;
use std::rc::Rc;
use std::result;

use callable::LoxCallable;
use instance::LoxInstance;
use parser::Var;

#[derive(Clone)]
pub enum Value {
    Str(String),
    Int(i64),
    Bool(bool),
    Nil,
    Callable(Rc<LoxCallable>),
    Instance(Rc<LoxInstance>),
}

#[derive(Debug)]
pub enum Error {
    Return(Value),
    RuntimeError { message: String, line: usize },
    TypeError(String),
    DivideByZero,
    UndefinedVar { var: Var, line: usize },
}

impl Error {
    pub fn line(&self) -> usize {
        match *self {
            Error::Return(_) | Error::TypeError(_) | Error::DivideByZero => 0,
            Error::RuntimeError { line, .. } | Error::UndefinedVar { line, .. } => line,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Return(ref value) => write!(f, "Return {}", value),
            Error::RuntimeError { ref message, .. } => write!(f, "{}", message),
            Error::TypeError(ref msg) => write!(f, "TypeError {}", msg),
            Error::DivideByZero => write!(f, "DivideByZero: division by zero"),
            Error::UndefinedVar { ref var, .. } => write!(f, "Undefined variable '{}'.", var.name),
        }
    }
}

pub type Result<T> = result::Result<T, Error>;
pub type ValueResult = Result<Value>;
