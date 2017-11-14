use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::mem;
use std::ops;
use std::rc::Rc;
use std::usize;

use callable::{Clock, LoxFunction};
use class::LoxClass;
use environment::Environment;
use parser::{BinOp, Expr, LogicOp, Stmt, UnaryOp};
use primitive::{Error, Result, Value, ValueResult};

pub struct Interpreter {
    globals: Environment,
    env: Environment,
}

impl Default for Interpreter {
    fn default() -> Self {
        let env = Environment::default();
        env.define("clock".into(), Value::Fun(Rc::new(Clock)));

        Self {
            globals: env.clone(),
            env: env,
        }
    }
}

impl Interpreter {
    pub fn execute(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            self.exec_stmt(stmt)?;
        }
        Ok(())
    }

    pub fn exec_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match *stmt {
            Stmt::Expr(ref expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::Print(ref expr) => {
                println!("{}", self.evaluate(expr)?);
                Ok(())
            }
            Stmt::VarDecl(ref name, ref initializer) => {
                let value = match *initializer {
                    Some(ref expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                };

                self.env.define(name.clone(), value);
                Ok(())
            }
            Stmt::Fun(ref fun) => {
                let func = LoxFunction::new(fun.clone(), self.env.clone(), false);
                self.env.define(fun.name.clone(), Value::Fun(Rc::new(func)));
                Ok(())
            }
            Stmt::Class(ref class) => {
                self.env.define(class.name.clone(), Value::Nil);

                let mut methods = HashMap::new();
                for method in &class.methods {
                    let func =
                        LoxFunction::new(method.clone(), self.env.clone(), method.name == "init");
                    methods.insert(method.name.clone(), Rc::new(func));
                }
                let lox_class = LoxClass::new(class.name.clone(), methods);

                self.env.assign(&class.name, Value::Class(lox_class));
                Ok(())
            }
            Stmt::Return(ref expr) => {
                let value = if let Some(ref expr) = *expr {
                    self.evaluate(expr)?
                } else {
                    Value::Nil
                };

                Err(Error::Return(value))
            }
            Stmt::Block(ref stmts) => {
                let env = Environment::with_enclosing(self.env.clone());
                self.execute_block(stmts, env)
            }
            Stmt::If(ref condition, ref then_branch, ref else_branch) => {
                if is_truthy(&self.evaluate(condition)?) {
                    self.exec_stmt(then_branch)?;
                } else if let Some(ref branch) = *else_branch {
                    self.exec_stmt(branch)?;
                }
                Ok(())
            }
            Stmt::While(ref condition, ref body) => {
                while is_truthy(&self.evaluate(condition)?) {
                    self.exec_stmt(body)?;
                }
                Ok(())
            }
        }
    }

    pub fn evaluate(&mut self, expr: &Expr) -> ValueResult {
        match *expr {
            Expr::Binary(ref left_expr, op, ref right_expr) => {
                let left = self.evaluate(left_expr)?;
                let right = self.evaluate(right_expr)?;

                match op {
                    BinOp::Plus => left + right,
                    BinOp::Minus => left - right,
                    BinOp::Star => left * right,
                    BinOp::Slash => left / right,

                    BinOp::BangEqual => Ok(Value::Bool(!eq(left, right)?)),
                    BinOp::EqualEqual => Ok(Value::Bool(eq(left, right)?)),
                    BinOp::Greater => Ok(Value::Bool(cmp(left, right)? == Ordering::Greater)),
                    BinOp::GreaterEqual => {
                        let ord = cmp(left, right)?;
                        Ok(Value::Bool(
                            ord == Ordering::Greater || ord == Ordering::Equal,
                        ))
                    }
                    BinOp::Less => Ok(Value::Bool(cmp(left, right)? == Ordering::Less)),
                    BinOp::LessEqual => {
                        let ord = cmp(left, right)?;
                        Ok(Value::Bool(ord == Ordering::Less || ord == Ordering::Equal))
                    }
                }
            }
            Expr::Logical(ref left_expr, op, ref right_expr) => {
                let left = self.evaluate(left_expr)?;

                match op {
                    LogicOp::And => if is_truthy(&left) {
                        self.evaluate(right_expr)
                    } else {
                        Ok(left)
                    },
                    LogicOp::Or => if is_truthy(&left) {
                        Ok(left)
                    } else {
                        self.evaluate(right_expr)
                    },
                }
            }
            Expr::Grouping(ref expr) => self.evaluate(expr),
            Expr::Literal(ref value) => Ok(value.clone()),
            Expr::Unary(op, ref expr) => {
                let value = self.evaluate(expr)?;

                match op {
                    UnaryOp::Minus => match value {
                        Value::Int(n) => Ok(Value::Int(-n)),
                        val => panic!("Cannot use unary - on {:?}", val),
                    },
                    UnaryOp::Bang => Ok(Value::Bool(!is_truthy(&value))),
                }
            }
            Expr::Var(ref name, hops) => self.look_up_variable(name, hops),
            Expr::VarAssign(ref name, ref expr, hops) => {
                let value = self.evaluate(expr)?;

                match hops {
                    Some(hops) => {
                        self.env.assign_at(name, value.clone(), hops);
                        Ok(value)
                    }
                    None => if self.globals.assign(name, value.clone()) {
                        Ok(value)
                    } else {
                        Err(Error::UndefinedVar(name.clone()))
                    },
                }
            }
            Expr::Call(ref callee, ref argument_exprs) => {
                let callee = self.evaluate(callee)?;

                let mut arguments = Vec::with_capacity(argument_exprs.len());
                for expr in argument_exprs {
                    arguments.push(self.evaluate(expr)?);
                }

                let f = callee.unwrap_callable();

                if arguments.len() == f.arity() {
                    f.call(self, arguments)
                } else {
                    Err(Error::ArityError(format!(
                        "fun takes {} arguments but got {}",
                        f.arity(),
                        arguments.len(),
                    )))
                }
            }
            Expr::AnonymousFun(ref fun) => {
                let callable = LoxFunction::new(fun.clone(), self.env.clone(), false);
                Ok(Value::Fun(Rc::new(callable)))
            }
            Expr::Get(ref expr, ref name) => {
                let object = self.evaluate(expr)?;
                if let Value::Instance(instance) = object {
                    instance.get(name)
                } else {
                    Err(Error::TypeError(
                        format!("Unable to get {} property on {}", name, object),
                    ))
                }
            }
            Expr::Set(ref left, ref name, ref right) => {
                let object = self.evaluate(left)?;
                if let Value::Instance(mut instance) = object {
                    let value = self.evaluate(right)?;
                    instance.set(name.clone(), value.clone());
                    Ok(value)
                } else {
                    Err(Error::TypeError(
                        format!("Unable to set {} property on {}", name, object),
                    ))
                }
            }
            Expr::This(hops) => self.look_up_variable("this", hops),
        }
    }

    pub fn execute_block(&mut self, block: &[Stmt], env: Environment) -> Result<()> {
        let previous = mem::replace(&mut self.env, env);

        let mut result = Ok(());
        for stmt in block {
            result = self.exec_stmt(stmt);
            if result.is_err() {
                break;
            }
        }

        // Restore previous environment.
        self.env = previous;
        result
    }

    fn look_up_variable(&self, name: &str, hops: Option<usize>) -> ValueResult {
        match hops {
            Some(hops) => Ok(self.env.get_at(name, hops)),
            None => match self.globals.get(name) {
                Some(value) => Ok(value),
                None => Err(Error::UndefinedVar(name.into())),
            },
        }
    }
}

fn is_truthy(value: &Value) -> bool {
    match *value {
        Value::Str(ref s) => !s.is_empty(),
        Value::Int(n) => n != 0,
        Value::Bool(b) => b,
        Value::Nil => false,
        Value::Fun(..) | Value::Class(..) | Value::Instance(..) => true,
    }
}

impl ops::Add for Value {
    type Output = ValueResult;
    fn add(self, rhs: Value) -> ValueResult {
        match (self, rhs) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left + right)),
            (Value::Str(left), Value::Str(right)) => Ok(Value::Str(left + &right)),
            (left, right) => Err(Error::TypeError(
                format!("Cannot add {:?} to {:?}", left, right),
            )),
        }
    }
}

impl ops::Sub for Value {
    type Output = ValueResult;
    fn sub(self, rhs: Value) -> ValueResult {
        match (self, rhs) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left - right)),
            (left, right) => Err(Error::TypeError(
                format!("Cannot subtract {:?} to {:?}", left, right),
            )),
        }
    }
}

impl ops::Mul for Value {
    type Output = ValueResult;
    fn mul(self, rhs: Value) -> ValueResult {
        match (self, rhs) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left * right)),
            (left, right) => Err(Error::TypeError(
                format!("Cannot multiply {:?} to {:?}", left, right),
            )),
        }
    }
}

impl ops::Div for Value {
    type Output = ValueResult;
    fn div(self, rhs: Value) -> ValueResult {
        match (self, rhs) {
            (Value::Int(_), Value::Int(0)) => Err(Error::DivideByZero),
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left / right)),
            (left, right) => Err(Error::TypeError(
                format!("Cannot divide {:?} to {:?}", left, right),
            )),
        }
    }
}

fn eq(left: Value, right: Value) -> Result<bool> {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => Ok(left == right),
        (Value::Str(ref left), Value::Str(ref right)) => Ok(left == right),
        (left, right) => Err(Error::TypeError(
            format!("Cannot compare {:?} to {:?}", left, right),
        )),
    }
}

fn cmp(left: Value, right: Value) -> Result<Ordering> {
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => Ok(left.cmp(&right)),
        (Value::Str(left), Value::Str(right)) => Ok(left.cmp(&right)),
        (left, right) => Err(Error::TypeError(
            format!("Cannot compare {:?} to {:?}", left, right),
        )),
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Str(ref s) => write!(f, "{}", s),
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Fun(ref func) => write!(f, "<fun {}>", func.name()),
            Value::Class(ref class) => write!(f, "<class {}>", class.name()),
            Value::Instance(ref instance) => write!(f, "<{} instance>", instance.class_name()),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
