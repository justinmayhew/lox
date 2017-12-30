use std::fmt;

use parser::Var;
use super::Value;

#[derive(Debug)]
pub enum Error {
    Return(Value),
    Message { message: String, line: usize },
    UndefinedVar(Var),
}

impl Error {
    pub fn line(&self) -> usize {
        match *self {
            Error::Return(_) => unreachable!(),
            Error::Message { line, .. } => line,
            Error::UndefinedVar(ref var) => var.line(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Return(_) => unreachable!(),
            Error::Message { ref message, .. } => write!(f, "{}", message),
            Error::UndefinedVar(ref var) => write!(f, "Undefined variable '{}'", var.name()),
        }
    }
}

macro_rules! err {
    ($line:expr, $($tt:tt)*) => ({
        let message = Error::Message { message: format!($($tt)*), line: $line };
        return Err(message);
    })
}
