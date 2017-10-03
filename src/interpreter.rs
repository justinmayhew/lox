use std::cmp::Ordering;
use std::mem;
use std::ops;
use std::result;

use parser::{BinOp, Expr, Stmt, UnaryOp, Value};
use environment::Environment;

#[derive(Debug)]
pub enum Error {
    TypeError(String),
    DivideByZero,
    UndefinedVar(String),
}

type Result<T> = result::Result<T, Error>;
type ValueResult = Result<Value>;

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(None),
        }
    }

    pub fn execute(&mut self, stmts: Vec<Stmt>) -> Result<()> {
        for stmt in stmts {
            self.exec_stmt(stmt)?;
        }
        Ok(())
    }

    fn exec_stmt(&mut self, stmt: Stmt) -> Result<()> {
        match stmt {
            Stmt::Expr(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::Print(expr) => {
                println!("{}", self.evaluate(expr)?);
                Ok(())
            }
            Stmt::VarDecl(name, initializer) => {
                let value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                };

                self.env.define(name, value);
                Ok(())
            }
            Stmt::Block(stmts) => {
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
            Stmt::If(condition, then_branch, else_branch) => {
                if is_truthy(self.evaluate(condition)?) {
                    self.exec_stmt(*then_branch)?;
                } else if let Some(branch) = else_branch {
                    self.exec_stmt(*branch)?;
                }
                Ok(())
            }
        }
    }

    pub fn evaluate(&mut self, expr: Expr) -> ValueResult {
        match expr {
            Expr::Binary(left_expr, op, right_expr) => {
                let left = self.evaluate(*left_expr)?;
                let right = self.evaluate(*right_expr)?;

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
            Expr::Grouping(expr) => self.evaluate(*expr),
            Expr::Literal(value) => Ok(value),
            Expr::Unary(op, expr) => {
                let value = self.evaluate(*expr)?;

                match op {
                    UnaryOp::Minus => match value {
                        Value::Int(n) => Ok(Value::Int(-n)),
                        val => panic!("Cannot use unary - on {:?}", val),
                    },
                    UnaryOp::Bang => Ok(Value::Bool(!is_truthy(value))),
                }
            }
            Expr::Var(name) => match self.env.get(&name) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::UndefinedVar(name)),
            },
            Expr::VarAssign(name, expr) => {
                let value = self.evaluate(*expr)?;

                if self.env.assign(&name, value.clone()) {
                    Ok(value)
                } else {
                    Err(Error::UndefinedVar(name))
                }
            }
        }
    }
}

fn is_truthy(value: Value) -> bool {
    match value {
        Value::Str(s) => !s.is_empty(),
        Value::Int(n) => n != 0,
        Value::Bool(b) => b,
        Value::Nil => false,
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
