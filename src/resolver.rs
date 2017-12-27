use std::collections::HashMap;

use parser::{self, Expr, ExprNode, Identifier, Stmt};

pub fn resolve(stmts: &mut [Stmt]) -> Result<(), ()> {
    let mut resolver = Resolver::default();
    for stmt in stmts {
        resolver.resolve_stmt(stmt);
    }

    if resolver.has_error {
        Err(())
    } else {
        Ok(())
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
    Subclass,
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
    has_error: bool,
}

impl Default for Resolver {
    fn default() -> Self {
        Self {
            scopes: Vec::new(),
            current_fn: FunctionKind::None,
            current_class: ClassKind::None,
            has_error: false,
        }
    }
}

impl Resolver {
    fn resolve_stmt(&mut self, stmt: &mut Stmt) {
        match *stmt {
            Stmt::Expr(ref mut expr) | Stmt::Print(ref mut expr) => self.resolve_expr(expr),
            Stmt::VarDecl(ref identifier, ref mut expr) => {
                self.declare(identifier);
                if let Some(ref mut expr) = *expr {
                    self.resolve_expr(expr);
                }
                self.define(identifier);
            }
            Stmt::Fun(ref mut fun) => {
                self.declare(&fun.identifier);
                self.define(&fun.identifier);
                self.resolve_function(&mut fun.parameters, &mut fun.body, FunctionKind::Function);
            }
            Stmt::Class(ref mut class) => {
                self.declare(&class.identifier);
                self.define(&class.identifier);

                let enclosing_class = self.current_class;

                if let Some(ref mut superclass) = class.superclass {
                    self.current_class = ClassKind::Subclass;
                    self.resolve_var(superclass);
                    self.begin_scope(Some("super".into()));
                } else {
                    self.current_class = ClassKind::Class;
                }

                self.begin_scope(Some("this".into()));

                for method in &mut class.methods {
                    let declaration = if method.name() == "init" {
                        FunctionKind::Initializer
                    } else {
                        FunctionKind::Method
                    };
                    self.resolve_function(&mut method.parameters, &mut method.body, declaration);
                }

                self.end_scope();

                if class.superclass.is_some() {
                    self.end_scope();
                }

                self.current_class = enclosing_class;
            }
            Stmt::Return(ref mut expr, line) => {
                if let FunctionKind::None = self.current_fn {
                    self.error(line, "return", "Cannot return from top-level code");
                }

                if let Some(ref mut expr) = *expr {
                    if let FunctionKind::Initializer = self.current_fn {
                        self.error(line, "return", "Cannot return a value from an initializer");
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
            Expr::Binary(ref mut left, _, ref mut right)
            | Expr::Logical(ref mut left, _, ref mut right) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Grouping(ref mut expr)
            | Expr::Unary(_, ref mut expr)
            | Expr::Get(ref mut expr, _) => self.resolve_expr(expr),
            Expr::Literal(_) => {}
            Expr::Var(ref mut var) => {
                let mut is_initializing = false;

                if let Some(map) = self.scopes.last() {
                    if let Some(v) = map.get(var.name()) {
                        if !v.is_initialized() {
                            is_initializing = true;
                        }
                    }
                }

                if is_initializing {
                    self.error(
                        var.line(),
                        var.name(),
                        "Cannot read local variable in its own initializer",
                    );
                }

                self.resolve_var(var);
            }
            Expr::VarAssign(ref mut var, ref mut expr) => {
                self.resolve_expr(expr);
                self.resolve_var(var);
            }
            Expr::Call(ref mut callee, ref mut args) => {
                self.resolve_expr(callee);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::Fun(ref mut fun) => {
                self.resolve_function(&mut fun.parameters, &mut fun.body, FunctionKind::Anonymous);
            }
            Expr::Set(ref mut object, _, ref mut value) => {
                self.resolve_expr(value);
                self.resolve_expr(object);
            }
            Expr::Super(ref mut super_var, _) => {
                if let ClassKind::None = self.current_class {
                    self.error(
                        super_var.line(),
                        "super",
                        "Cannot use 'super' outside of a class",
                    );
                } else if let ClassKind::Class = self.current_class {
                    self.error(
                        super_var.line(),
                        "super",
                        "Cannot use 'super' in a class with no superclass",
                    );
                }

                self.resolve_var(super_var);
            }
            Expr::This(ref mut var) => {
                if let ClassKind::None = self.current_class {
                    self.error(var.line(), "this", "Cannot use 'this' outside of a class");
                }
                self.resolve_var(var);
            }
        }
    }

    fn resolve_function(
        &mut self,
        params: &mut [Identifier],
        body: &mut [Stmt],
        kind: FunctionKind,
    ) {
        let enclosing_fn = self.current_fn;
        self.current_fn = kind;

        self.begin_scope(None);
        for param in params {
            self.declare(param);
            self.define(param);
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

    fn declare(&mut self, identifier: &Identifier) {
        let mut already_declared = false;

        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&identifier.name) {
                already_declared = true;
            }
            scope.insert(identifier.name.clone(), Var::uninitialized());
        }

        if already_declared {
            self.error(
                identifier.line,
                &identifier.name,
                "Variable with this name already declared in this scope",
            );
        }
    }

    fn define(&mut self, identifier: &Identifier) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(identifier.name.clone(), Var::initialized());
        }
    }

    fn resolve_var(&mut self, var: &mut parser::Var) {
        for (i, scope) in self.scopes.iter_mut().rev().enumerate() {
            if let Some(ref mut v) = scope.get_mut(var.name()) {
                v.increment_usages();
                var.hops = i;
                return;
            }
        }

        // Not found. Assume it is global.
        var.hops = self.scopes.len();
    }

    fn error(&mut self, line: usize, location: &str, message: &str) {
        eprintln!("[line {}] Error at '{}': {}.", line, location, message);
        self.has_error = true;
    }
}
