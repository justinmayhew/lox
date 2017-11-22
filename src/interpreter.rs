use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use callable::{Clock, LoxFunction};
use class::LoxClass;
use environment::Environment;
use parser::{BinOp, Expr, ExprNode, LogicOp, Stmt, UnaryOp, Var};
use primitive::{Error, Result, Value, ValueResult};

pub struct Interpreter {
    globals: Environment,
    env: Environment,
}

impl Default for Interpreter {
    fn default() -> Self {
        let env = Environment::default();
        env.define("clock".into(), Value::Callable(Rc::new(Clock)));

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
                self.env
                    .define(fun.name.clone(), Value::Callable(Rc::new(func)));
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
                let lox_class = Rc::new(LoxClass::new(class.name.clone(), methods));

                self.env.assign(&class.name, Value::Callable(lox_class));
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
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                        (Value::Str(a), Value::Str(b)) => Ok(Value::Str(a + &b)),
                        _ => Err(Error::RuntimeError {
                            message: "Operands must be two numbers or two strings.".into(),
                            line,
                        }),
                    },
                    BinOp::Minus => with_numbers(a, b, line, |a, b| Ok(Value::Int(a - b))),
                    BinOp::Star => with_numbers(a, b, line, |a, b| Ok(Value::Int(a * b))),
                    BinOp::Slash => with_numbers(a, b, line, |a, b| {
                        if b == 0 {
                            Err(Error::DivideByZero)
                        } else {
                            Ok(Value::Int(a / b))
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
                    UnaryOp::Minus => match value {
                        Value::Int(n) => Ok(Value::Int(-n)),
                        val => panic!("Cannot use unary - on {:?}", val),
                    },
                    UnaryOp::Bang => Ok(Value::Bool(!is_truthy(&value))),
                }
            }
            Expr::Var(ref var) | Expr::This(ref var) => match self.look_up_variable(var) {
                Some(value) => Ok(value),
                None => Err(Error::UndefinedVar {
                    var: var.clone(),
                    line: node.line(),
                }),
            },
            Expr::VarAssign(ref var, ref expr) => {
                let value = self.evaluate(expr)?;

                match var.hops {
                    Some(hops) => {
                        self.env.assign_at(&var.name, value.clone(), hops);
                        Ok(value)
                    }
                    None => if self.globals.assign(&var.name, value.clone()) {
                        Ok(value)
                    } else {
                        Err(Error::UndefinedVar {
                            var: var.clone(),
                            line: node.line(),
                        })
                    },
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
            Expr::AnonymousFun(ref fun) => {
                let callable = LoxFunction::new(fun.clone(), self.env.clone(), false);
                Ok(Value::Callable(Rc::new(callable)))
            }
            Expr::Get(ref expr, ref name) => {
                let value = self.evaluate(expr)?;

                if let Value::Instance(instance) = value {
                    if let Some(value) = instance.get_field(name) {
                        Ok(value)
                    } else if let Some(method) =
                        instance.class().get_method(name, Rc::clone(&instance))
                    {
                        Ok(Value::Callable(method))
                    } else {
                        Err(Error::RuntimeError {
                            message: format!("Undefined property '{}'.", name),
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
            Expr::Set(ref left, ref name, ref right) => {
                let object = self.evaluate(left)?;
                if let Value::Instance(instance) = object {
                    let value = self.evaluate(right)?;
                    instance.set(name.clone(), value.clone());
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

    fn look_up_variable(&self, var: &Var) -> Option<Value> {
        match var.hops {
            Some(hops) => Some(self.env.get_at(&var.name, hops)),
            None => match self.globals.get(&var.name) {
                Some(value) => Some(value),
                None => None,
            },
        }
    }
}

fn is_truthy(value: &Value) -> bool {
    match *value {
        Value::Callable(_) | Value::Instance(_) | Value::Str(_) | Value::Int(_) => true,
        Value::Bool(b) => b,
        Value::Nil => false,
    }
}

fn with_numbers<F>(left: Value, right: Value, line: usize, f: F) -> ValueResult
where
    F: Fn(i64, i64) -> ValueResult,
{
    match (left, right) {
        (Value::Int(left), Value::Int(right)) => f(left, right),
        _ => Err(Error::RuntimeError {
            message: "Operands must be numbers.".into(),
            line,
        }),
    }
}

fn is_equal(left: Value, right: Value) -> bool {
    match (left, right) {
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Callable(a), Value::Callable(b)) => Rc::ptr_eq(&a, &b),
        (Value::Instance(a), Value::Instance(b)) => Rc::ptr_eq(&a, &b),
        _ => false,
    }
}
