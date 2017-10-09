use std::collections::HashMap;

use parser::{Expr, Stmt};

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub fn resolve_stmt(&mut self, stmt: &mut Stmt) {
        match *stmt {
            Stmt::Expr(ref mut expr) | Stmt::Print(ref mut expr) => self.resolve_expr(expr),
            Stmt::VarDecl(ref name, ref mut expr) => {
                self.declare(name.clone());
                if let Some(ref mut expr) = *expr {
                    self.resolve_expr(expr);
                }
                self.define(name.clone());
            }
            Stmt::Fun(ref name, ref mut params, ref mut body) => {
                self.declare(name.clone());
                self.define(name.clone());
                self.resolve_function(params, body);
            }
            Stmt::Return(ref mut expr) => if let Some(ref mut expr) = *expr {
                self.resolve_expr(expr);
            },
            Stmt::Block(ref mut body) => {
                self.begin_scope();
                for stmt in body {
                    self.resolve_stmt(stmt);
                }
                self.end_scope();
            }
            Stmt::If(ref mut condition, ref mut then_clause, ref mut else_clause) => {
                self.resolve_expr(condition);
                self.resolve_stmt(then_clause);
                if let Some(ref mut else_clause) = *else_clause {
                    self.resolve_stmt(else_clause);
                }
            }
            Stmt::While(ref mut condition, ref mut body) => {
                self.resolve_expr(condition);
                self.resolve_stmt(body);
            }
        }
    }

    fn resolve_expr(&mut self, expr: &mut Expr) {
        match *expr {
            Expr::Binary(ref mut left, _op, ref mut right) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Logical(ref mut left, _op, ref mut right) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Grouping(ref mut expr) => self.resolve_expr(expr),
            Expr::Literal(ref _value) => {}
            Expr::Unary(_op, ref mut expr) => self.resolve_expr(expr),
            Expr::Var(ref name, ref mut hops) => {
                if let Some(map) = self.scopes.last() {
                    if map.get(name) == Some(&false) {
                        panic!("Cannot read local variable in its own initializer.");
                    }
                }
                self.resolve_local(name, hops);
            }
            Expr::VarAssign(ref name, ref mut expr, ref mut hops) => {
                self.resolve_expr(expr);
                self.resolve_local(name, hops);
            }
            Expr::Call(ref mut callee, ref mut args) => {
                self.resolve_expr(callee);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::AnonymousFun(ref mut params, ref mut body) => {
                self.resolve_function(params, body);
            }
        }
    }

    fn resolve_function(&mut self, params: &mut [String], body: &mut [Stmt]) {
        self.begin_scope();
        for param in params {
            self.declare(param.clone());
            self.define(param.clone());
        }
        for stmt in body {
            self.resolve_stmt(stmt);
        }
        self.end_scope();
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: String) {
        if self.scopes.is_empty() {
            return;
        }

        let scope = self.scopes.last_mut().unwrap();
        scope.insert(name, false);
    }

    fn define(&mut self, name: String) {
        if self.scopes.is_empty() {
            return;
        }

        let scope = self.scopes.last_mut().unwrap();
        scope.insert(name, true);
    }

    fn resolve_local(&mut self, name: &str, hops: &mut usize) {
        if self.scopes.is_empty() {
            return;
        }

        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(name) {
                *hops = self.scopes.len() - 1 - i;
                return;
            }
        }

        // Not found. Assume it is global.
    }
}
