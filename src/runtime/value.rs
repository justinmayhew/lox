use std::fmt;
use std::rc::Rc;

use super::{LoxCallable, LoxClass, LoxInstance};

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
            Value::String(_)
            | Value::Number(_)
            | Value::Bool(_)
            | Value::Nil
            | Value::Instance(_) => None,
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
