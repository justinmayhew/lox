use std::fmt;
use std::rc::Rc;
use std::result;

use callable::LoxCallable;
use class::LoxClass;
use instance::LoxInstance;
use parser::Var;

#[derive(Clone, Debug)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    Callable(Rc<LoxCallable>),
    Class(Rc<LoxClass>),
    Instance(Rc<LoxInstance>),
}

impl Value {
    pub fn to_callable(self) -> Option<Rc<LoxCallable>> {
        match self {
            Value::Callable(f) => Some(f),
            Value::Class(c) => Some(c),
            _ => None,
        }
    }

    pub fn to_class(self) -> Option<Rc<LoxClass>> {
        if let Value::Class(class) = self {
            Some(class)
        } else {
            None
        }
    }

    pub fn unwrap_instance(self) -> Rc<LoxInstance> {
        if let Value::Instance(instance) = self {
            instance
        } else {
            panic!("Value is not an instance: {:?}", self);
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::String(ref s) => write!(f, "{}", s),
            Value::Number(n) => if n == 0.0 && n.is_sign_negative() {
                // The JVM prints negative zero with a sign.
                write!(f, "-{}", n)
            } else {
                write!(f, "{}", n)
            },
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Callable(ref callable) => write!(f, "{}", callable.to_string()),
            Value::Class(ref class) => write!(f, "{}", class.name()),
            Value::Instance(ref instance) => write!(f, "{} instance", instance.class().name()),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Return(Value),
    RuntimeError { message: String, line: usize },
    TypeError(String),
    DivideByZero,
    UndefinedVar(Var),
}

impl Error {
    pub fn line(&self) -> usize {
        match *self {
            Error::Return(_) | Error::TypeError(_) | Error::DivideByZero => 0,
            Error::RuntimeError { line, .. } => line,
            Error::UndefinedVar(ref var) => var.line(),
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
            Error::UndefinedVar(ref var) => write!(f, "Undefined variable '{}'.", var.name()),
        }
    }
}

pub type Result<T> = result::Result<T, Error>;
pub type ValueResult = Result<Value>;
