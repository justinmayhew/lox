use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use callable::{Clock, Function, LoxCallable, LoxFunction};
use class::LoxClass;
use environment::Environment;
use parser::{BinOp, Expr, ExprNode, LogicOp, Stmt, UnaryOp};
use primitive::{Error, Result, Value, ValueResult};

pub struct Interpreter {
    env: Environment,
}

impl Default for Interpreter {
    fn default() -> Self {
        let env = Environment::default();
        env.define(Clock.name().into(), Value::Callable(Rc::new(Clock)));
        Self { env }
    }
}

impl Interpreter {
    pub fn execute(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            self.exec_stmt(stmt)?;
        }
        Ok(())
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match *stmt {
            Stmt::Expr(ref expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::Print(ref expr) => {
                println!("{}", self.evaluate(expr)?);
                Ok(())
            }
            Stmt::VarDecl(ref identifier, ref initializer) => {
                let value = match *initializer {
                    Some(ref expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                };

                self.env.define(identifier.name.clone(), value);
                Ok(())
            }
            Stmt::Fun(ref fun) => {
                let func = LoxFunction::new(Function::Decl(fun.clone()), self.env.clone(), false);
                self.env
                    .define(fun.name().into(), Value::Callable(Rc::new(func)));
                Ok(())
            }
            Stmt::Class(ref class) => {
                self.env.define(class.name().into(), Value::Nil);

                let mut methods = HashMap::new();
                for method in &class.methods {
                    let func = LoxFunction::new(
                        Function::Decl(method.clone()),
                        self.env.clone(),
                        method.name() == "init",
                    );
                    methods.insert(method.name().into(), Rc::new(func));
                }
                let lox_class = Rc::new(LoxClass::new(class.name().into(), methods));

                self.env
                    .assign(class.name().into(), Value::Callable(lox_class));
                Ok(())
            }
            Stmt::Return(ref expr, _) => {
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

    fn evaluate(&mut self, node: &ExprNode) -> ValueResult {
        match *node.expr() {
            Expr::Binary(ref left_expr, op, ref right_expr) => {
                let line = left_expr.line();
                let a = self.evaluate(left_expr)?;
                let b = self.evaluate(right_expr)?;

                match op {
                    BinOp::EqualEqual => Ok(Value::Bool(is_equal(a, b))),
                    BinOp::BangEqual => Ok(Value::Bool(!is_equal(a, b))),
                    BinOp::Plus => match (a, b) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                        (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                        _ => Err(Error::RuntimeError {
                            message: "Operands must be two numbers or two strings.".into(),
                            line,
                        }),
                    },
                    BinOp::Minus => with_numbers(a, b, line, |a, b| Ok(Value::Number(a - b))),
                    BinOp::Star => with_numbers(a, b, line, |a, b| Ok(Value::Number(a * b))),
                    BinOp::Slash => with_numbers(a, b, line, |a, b| {
                        if b == 0.0 {
                            Err(Error::DivideByZero)
                        } else {
                            Ok(Value::Number(a / b))
                        }
                    }),
                    BinOp::Greater => with_numbers(a, b, line, |a, b| Ok(Value::Bool(a > b))),
                    BinOp::GreaterEqual => with_numbers(a, b, line, |a, b| Ok(Value::Bool(a >= b))),
                    BinOp::Less => with_numbers(a, b, line, |a, b| Ok(Value::Bool(a < b))),
                    BinOp::LessEqual => with_numbers(a, b, line, |a, b| Ok(Value::Bool(a <= b))),
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
                    UnaryOp::Minus => if let Value::Number(n) = value {
                        Ok(Value::Number(-n))
                    } else {
                        Err(Error::RuntimeError {
                            message: "Operand must be a number.".into(),
                            line: expr.line(),
                        })
                    },
                    UnaryOp::Bang => Ok(Value::Bool(!is_truthy(&value))),
                }
            }
            Expr::Var(ref var) | Expr::This(ref var) => self.env
                .ancestor(var.hops)
                .get(var.name())
                .ok_or_else(|| Error::UndefinedVar {
                    var: var.clone(),
                    line: node.line(),
                }),
            Expr::VarAssign(ref var, ref expr) => {
                let value = self.evaluate(expr)?;
                let env = self.env.ancestor(var.hops);

                if env.assign(var.name().into(), value.clone()) {
                    Ok(value)
                } else {
                    Err(Error::UndefinedVar {
                        var: var.clone(),
                        line: node.line(),
                    })
                }
            }
            Expr::Call(ref callee, ref argument_exprs) => {
                let line = callee.line();
                let callee = self.evaluate(callee)?;

                let mut arguments = Vec::with_capacity(argument_exprs.len());
                for expr in argument_exprs {
                    arguments.push(self.evaluate(expr)?);
                }

                if let Value::Callable(f) = callee {
                    if f.arity() == arguments.len() {
                        f.call(self, arguments)
                    } else {
                        Err(Error::RuntimeError {
                            message: format!(
                                "Expected {} arguments but got {}.",
                                f.arity(),
                                arguments.len()
                            ),
                            line,
                        })
                    }
                } else {
                    Err(Error::RuntimeError {
                        message: "Can only call functions and classes.".into(),
                        line,
                    })
                }
            }
            Expr::Fun(ref fun) => {
                let callable =
                    LoxFunction::new(Function::Expr(fun.clone()), self.env.clone(), false);
                Ok(Value::Callable(Rc::new(callable)))
            }
            Expr::Get(ref expr, ref identifier) => {
                let value = self.evaluate(expr)?;

                if let Value::Instance(instance) = value {
                    if let Some(value) = instance.get_field(&identifier.name) {
                        Ok(value)
                    } else if let Some(method) = instance
                        .class()
                        .get_method(&identifier.name, Rc::clone(&instance))
                    {
                        Ok(Value::Callable(method))
                    } else {
                        Err(Error::RuntimeError {
                            message: format!("Undefined property '{}'.", identifier.name),
                            line: expr.line(),
                        })
                    }
                } else {
                    Err(Error::RuntimeError {
                        message: "Only instances have properties.".into(),
                        line: expr.line(),
                    })
                }
            }
            Expr::Set(ref left, ref identifier, ref right) => {
                let object = self.evaluate(left)?;
                if let Value::Instance(instance) = object {
                    let value = self.evaluate(right)?;
                    instance.set(identifier.name.clone(), value.clone());
                    Ok(value)
                } else {
                    Err(Error::RuntimeError {
                        message: "Only instances have fields.".into(),
                        line: left.line(),
                    })
                }
            }
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
}

fn is_truthy(value: &Value) -> bool {
    match *value {
        Value::Callable(_) | Value::Instance(_) | Value::String(_) | Value::Number(_) => true,
        Value::Bool(b) => b,
        Value::Nil => false,
    }
}

fn with_numbers<F>(left: Value, right: Value, line: usize, f: F) -> ValueResult
where
    F: Fn(f64, f64) -> ValueResult,
{
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => f(left, right),
        _ => Err(Error::RuntimeError {
            message: "Operands must be numbers.".into(),
            line,
        }),
    }
}

fn is_equal(left: Value, right: Value) -> bool {
    match (left, right) {
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Number(a), Value::Number(b)) => a.eq(&b),
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Callable(a), Value::Callable(b)) => Rc::ptr_eq(&a, &b),
        (Value::Instance(a), Value::Instance(b)) => Rc::ptr_eq(&a, &b),
        _ => false,
    }
}
