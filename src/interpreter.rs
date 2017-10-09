use std::cmp::Ordering;
use std::fmt;
use std::mem;
use std::ops;
use std::rc::Rc;

use environment::Environment;
use parser::{BinOp, Expr, LogicOp, Stmt, UnaryOp};
use primitive::{Error, Result, Value, ValueResult};
use callable::{Clock, LoxFunction};

pub struct Interpreter {
    pub env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = Environment::new(None);

        env.define("clock".into(), Value::Fun(Rc::new(Clock)));

        Self { env }
    }

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
            Stmt::Fun(ref name, ref parameters, ref body) => {
                let lox_function = LoxFunction::new(name.clone(), parameters.clone(), body.clone());
                self.env
                    .define(name.clone(), Value::Fun(Rc::new(lox_function)));
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
                // Set up new environment for this block.
                let previous = mem::replace(&mut self.env, Environment::new(None));
                self.env.set_enclosing(previous);

                let mut result = Ok(());

                for stmt in stmts {
                    result = self.exec_stmt(stmt);
                    if result.is_err() {
                        break;
                    }
                }

                // Restore previous environment.
                let mut env = mem::replace(&mut self.env, Environment::new(None));
                let previous = env.pop_enclosing();
                self.env = previous;

                result
            }
            Stmt::If(ref condition, ref then_branch, ref else_branch) => {
                if is_truthy(&self.evaluate(condition)?) {
                    self.exec_stmt(&*then_branch)?;
                } else if let Some(ref branch) = *else_branch {
                    self.exec_stmt(&*branch)?;
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
                let left = self.evaluate(&*left_expr)?;
                let right = self.evaluate(&*right_expr)?;

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
                let left = self.evaluate(&*left_expr)?;

                match op {
                    LogicOp::And => if is_truthy(&left) {
                        self.evaluate(&*right_expr)
                    } else {
                        Ok(left)
                    },
                    LogicOp::Or => if is_truthy(&left) {
                        Ok(left)
                    } else {
                        self.evaluate(&*right_expr)
                    },
                }
            }
            Expr::Grouping(ref expr) => self.evaluate(&*expr),
            Expr::Literal(ref value) => Ok(value.clone()),
            Expr::Unary(op, ref expr) => {
                let value = self.evaluate(&*expr)?;

                match op {
                    UnaryOp::Minus => match value {
                        Value::Int(n) => Ok(Value::Int(-n)),
                        val => panic!("Cannot use unary - on {:?}", val),
                    },
                    UnaryOp::Bang => Ok(Value::Bool(!is_truthy(&value))),
                }
            }
            Expr::Var(ref name) => match self.env.get(name) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::UndefinedVar(name.clone())),
            },
            Expr::VarAssign(ref name, ref expr) => {
                let value = self.evaluate(&*expr)?;

                if self.env.assign(name, value.clone()) {
                    Ok(value)
                } else {
                    Err(Error::UndefinedVar(name.clone()))
                }
            }
            Expr::Call(ref callee, ref argument_exprs) => {
                let callee = self.evaluate(&*callee)?;

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
        }
    }
}

fn is_truthy(value: &Value) -> bool {
    match *value {
        Value::Str(ref s) => !s.is_empty(),
        Value::Int(n) => n != 0,
        Value::Bool(b) => b,
        Value::Nil => false,
        Value::Fun(..) => true,
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
            Value::Fun(ref func) => write!(f, "<fn {}>", func.name()),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
