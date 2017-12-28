use std::fmt;

use parser::Var;
use super::Value;

#[derive(Debug)]
pub enum Error {
    Return(Value),
    RuntimeError { message: String, line: usize },
    DivideByZero,
    UndefinedVar(Var),
}

impl Error {
    pub fn line(&self) -> usize {
        match *self {
            Error::Return(_) | Error::DivideByZero => 0,
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
            Error::DivideByZero => write!(f, "DivideByZero: division by zero"),
            Error::UndefinedVar(ref var) => write!(f, "Undefined variable '{}'.", var.name()),
        }
    }
}
