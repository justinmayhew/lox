use std::collections::HashMap;

use parser::{Expr, Stmt};

#[derive(Copy, Clone)]
enum FunctionKind {
    None,
    Function,
    Anonymous,
    Method,
}

#[derive(Copy, Clone)]
enum ClassKind {
    None,
    Class,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, (bool, usize)>>,
    current_fn: FunctionKind,
    current_class: ClassKind,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            current_fn: FunctionKind::None,
            current_class: ClassKind::None,
        }
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
            Stmt::Fun(ref mut fun) => {
                self.declare(fun.name.clone());
                self.define(fun.name.clone());
                self.resolve_function(&mut fun.parameters, &mut fun.body, FunctionKind::Function);
            }
            Stmt::Class(ref mut class) => {
                self.declare(class.name.clone());
                self.define(class.name.clone());

                let enclosing_class = self.current_class;
                self.current_class = ClassKind::Class;

                self.begin_scope();
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert("this".into(), (true, 0));

                for method in &mut class.methods {
                    let declaration = FunctionKind::Method;
                    self.resolve_function(&mut method.parameters, &mut method.body, declaration);
                }

                self.end_scope();

                self.current_class = enclosing_class;
            }
            Stmt::Return(ref mut expr) => {
                if let FunctionKind::None = self.current_fn {
                    panic!("Cannot return from top-level code.");
                }

                if let Some(ref mut expr) = *expr {
                    self.resolve_expr(expr);
                }
            }
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
                    if map.get(name) == Some(&(false, 0)) {
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
            Expr::AnonymousFun(ref mut fun) => {
                self.resolve_function(&mut fun.parameters, &mut fun.body, FunctionKind::Anonymous);
            }
            Expr::Get(ref mut expr, _) => {
                self.resolve_expr(expr);
            }
            Expr::Set(ref mut object, _, ref mut value) => {
                self.resolve_expr(value);
                self.resolve_expr(object);
            }
            Expr::This(ref mut hops) => {
                if let ClassKind::None = self.current_class {
                    panic!("Cannot use 'this' outside of a class.");
                }
                self.resolve_local("this", hops);
            }
        }
    }

    fn resolve_function(&mut self, params: &mut [String], body: &mut [Stmt], kind: FunctionKind) {
        let enclosing_fn = self.current_fn;
        self.current_fn = kind;

        self.begin_scope();
        for param in params {
            self.declare(param.clone());
            self.define(param.clone());
        }
        for stmt in body {
            self.resolve_stmt(stmt);
        }
        self.end_scope();

        self.current_fn = enclosing_fn;
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        {
            let scope = self.scopes.last_mut().unwrap();
            for (key, &(_, usages)) in scope.iter() {
                if usages == 0 && key != "this" {
                    eprintln!("Unused variable: {}", key);
                }
            }
        }

        self.scopes.pop();
    }

    fn declare(&mut self, name: String) {
        if self.scopes.is_empty() {
            return;
        }

        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&name) {
            panic!("Variable with this name is already declared in this scope.");
        }
        scope.insert(name, (false, 0));
    }

    fn define(&mut self, name: String) {
        if self.scopes.is_empty() {
            return;
        }

        let scope = self.scopes.last_mut().unwrap();
        scope.insert(name, (true, 0));
    }

    fn resolve_local(&mut self, name: &str, hops: &mut usize) {
        let len = self.scopes.len();

        for i in (0..len).rev() {
            if let Some(ref mut init_and_usage) = self.scopes[i].get_mut(name) {
                init_and_usage.1 += 1;
                *hops = len - 1 - i;
                return;
            }
        }

        // Not found. Assume it is global.
    }
}
