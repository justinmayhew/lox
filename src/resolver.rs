use std::collections::HashMap;

use parser::{self, Expr, ExprNode, Stmt};

pub fn resolve(stmts: &mut [Stmt]) {
    let mut resolver = Resolver::default();
    for stmt in stmts {
        resolver.resolve_stmt(stmt);
    }
}

#[derive(Copy, Clone)]
enum FunctionKind {
    None,
    Function,
    Initializer,
    Anonymous,
    Method,
}

#[derive(Copy, Clone)]
enum ClassKind {
    None,
    Class,
}

struct Var {
    initialized: bool,
    usages: usize,
}

impl Var {
    fn uninitialized() -> Self {
        Self {
            initialized: false,
            usages: 0,
        }
    }

    fn initialized() -> Self {
        Self {
            initialized: true,
            usages: 0,
        }
    }

    fn is_initialized(&self) -> bool {
        self.initialized
    }

    fn is_used(&self) -> bool {
        self.usages > 0
    }

    fn increment_usages(&mut self) {
        self.usages += 1;
    }
}

struct Resolver {
    scopes: Vec<HashMap<String, Var>>,
    current_fn: FunctionKind,
    current_class: ClassKind,
}

impl Default for Resolver {
    fn default() -> Self {
        Self {
            scopes: Vec::new(),
            current_fn: FunctionKind::None,
            current_class: ClassKind::None,
        }
    }
}

impl Resolver {
    fn resolve_stmt(&mut self, stmt: &mut Stmt) {
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

                self.begin_scope(Some("this".into()));

                for method in &mut class.methods {
                    let declaration = if method.name == "init" {
                        FunctionKind::Initializer
                    } else {
                        FunctionKind::Method
                    };
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
                    if let FunctionKind::Initializer = self.current_fn {
                        panic!("Cannot return a value from an initializer.");
                    }
                    self.resolve_expr(expr);
                }
            }
            Stmt::Block(ref mut body) => {
                self.begin_scope(None);
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

    fn resolve_expr(&mut self, node: &mut ExprNode) {
        match *node.expr_mut() {
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
            Expr::Var(ref mut var) => {
                if let Some(map) = self.scopes.last() {
                    if let Some(v) = map.get(&var.name) {
                        if !v.is_initialized() {
                            panic!("Cannot read local variable in its own initializer.");
                        }
                    }
                }
                self.resolve_local(var);
            }
            Expr::VarAssign(ref mut var, ref mut expr) => {
                self.resolve_expr(expr);
                self.resolve_local(var);
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
            Expr::This(ref mut var) => {
                if let ClassKind::None = self.current_class {
                    panic!("Cannot use 'this' outside of a class.");
                }
                self.resolve_local(var);
            }
        }
    }

    fn resolve_function(&mut self, params: &mut [String], body: &mut [Stmt], kind: FunctionKind) {
        let enclosing_fn = self.current_fn;
        self.current_fn = kind;

        self.begin_scope(None);
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

    fn begin_scope(&mut self, name: Option<String>) {
        let mut scope = HashMap::new();
        if let Some(name) = name {
            scope.insert(name, Var::initialized());
        }
        self.scopes.push(scope);
    }

    fn end_scope(&mut self) {
        for (name, var) in self.scopes.pop().expect("missing scope") {
            if !var.is_used() && name != "this" {
                warn!("Unused variable: {}", name);
            }
        }
    }

    fn declare(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name) {
                panic!("Variable with this name is already declared in this scope.");
            }
            scope.insert(name, Var::uninitialized());
        }
    }

    fn define(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, Var::initialized());
        }
    }

    fn resolve_local(&mut self, var: &mut parser::Var) {
        for (i, scope) in self.scopes.iter_mut().rev().enumerate() {
            if let Some(ref mut v) = scope.get_mut(&var.name) {
                v.increment_usages();
                var.hops = Some(i);
                return;
            }
        }

        // Not found. Assume it is global.
    }
}
