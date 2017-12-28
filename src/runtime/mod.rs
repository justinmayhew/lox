mod builtins;
mod environment;
mod error;
mod interpreter;
mod lox_callable;
mod lox_class;
mod lox_function;
mod lox_instance;
mod value;

pub use self::interpreter::Interpreter;
pub use self::value::Value;
use self::environment::Environment;
use self::error::Error;
use self::lox_callable::LoxCallable;
use self::lox_class::LoxClass;
use self::lox_function::LoxFunction;
use self::lox_instance::LoxInstance;

type Result<T> = ::std::result::Result<T, Error>;
