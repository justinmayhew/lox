use std::f64::{INFINITY, NEG_INFINITY};
use std::fmt;
use std::rc::Rc;

use super::{LoxCallable, LoxClass, LoxInstance};

#[derive(Clone, Debug)]
pub enum Value {
    String(::std::string::String),
    Number(f64),
    Bool(bool),
    Nil,
    Callable(Rc<LoxCallable>),
    Class(Rc<LoxClass>),
    Instance(Rc<LoxInstance>),
}
use self::Value::*;

impl Value {
    pub fn is_truthy(&self) -> bool {
        match *self {
            String(_) | Number(_) | Callable(_) | Class(_) | Instance(_) => true,
            Bool(b) => b,
            Nil => false,
        }
    }

    pub fn into_callable(self) -> Option<Rc<LoxCallable>> {
        match self {
            Callable(f) => Some(f),
            Class(c) => Some(c),
            String(_) | Number(_) | Bool(_) | Nil | Instance(_) => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (&String(ref a), &String(ref b)) => a == b,
            (&Number(a), &Number(b)) => a == b,
            (&Bool(a), &Bool(b)) => a == b,
            (&Nil, &Nil) => true,
            (&Callable(ref a), &Callable(ref b)) => Rc::ptr_eq(a, b),
            (&Class(ref a), &Class(ref b)) => Rc::ptr_eq(a, b),
            (&Instance(ref a), &Instance(ref b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            String(ref s) => write!(f, "{}", s),
            Number(n) => if n == INFINITY {
                write!(f, "Infinity")
            } else if n == NEG_INFINITY {
                write!(f, "-Infinity")
            } else if n == 0.0 && n.is_sign_negative() {
                write!(f, "-{}", n)
            } else {
                write!(f, "{}", n)
            },
            Bool(b) => write!(f, "{}", b),
            Nil => write!(f, "nil"),
            Callable(ref callable) => write!(f, "{}", callable),
            Class(ref class) => write!(f, "{}", class.name()),
            Instance(ref instance) => write!(f, "{} instance", instance.class().name()),
        }
    }
}
