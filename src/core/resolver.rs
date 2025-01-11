use crate::core::parser::{Expr, Stmt};
use crate::core::scanner::Token;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum VarState {
    // declared but not defined
    Declared,
    // initialized
    Defined,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncType {
    // normal function
    Function,
    // method function
    Method,
    // empty state
    None,
}

#[derive(Debug, Clone)]
pub struct Resolver<'a> {
    scopes: Vec<HashMap<&'a str, VarState>>,
    // <expression id, scope depth>
    locals: HashMap<usize, usize>,
    // tracks function context
    current_func: FuncType,
}

impl<'a> Resolver<'a> {
    /// initialize resolver
    pub fn new() -> Self {
        Resolver {
            scopes: Vec::new(),
            locals: HashMap::new(),
            current_func: FuncType::None,
        }
    }

    /// resolve the statements
    pub fn resolve(&mut self, stmts: &Vec<Stmt<'a>>) -> &HashMap<usize, usize> {
        for stmt in stmts {
            self.resolve_stmt(&stmt);
        }
        &self.locals
    }

    /// resolve a statement
    pub fn resolve_stmt(&mut self, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::Var { id, value, .. } => {
                let name = id.lexeme;
                self.declare(name);
                self.resolve_expr(value);
                self.define(name);
            }
            Stmt::Block { stmts, .. } => self.resolve_block(stmts),
            Stmt::Expr(expr) => self.resolve_expr(expr),
            Stmt::Fn {
                id, params, body, ..
            } => {
                let name = id.lexeme;
                self.declare(name);
                self.define(name);
                self.resolve_function(params, body, FuncType::Function);
            }
            Stmt::Print(expr) => self.resolve_expr(expr),
            Stmt::Return(expr) => self.resolve_expr(expr),
            Stmt::If { pred, body, else_b } => {
                self.resolve_expr(pred);
                self.begin_scope();
                self.resolve_stmt(body);
                self.end_scope();
                if let Some(else_b) = else_b {
                    self.begin_scope();
                    self.resolve_stmt(else_b);
                    self.end_scope();
                }
            }
            Stmt::While { pred, body } => {
                self.resolve_expr(pred);
                self.begin_scope();
                self.resolve_stmt(body);
                self.end_scope();
            }
            Stmt::Method {
                params, body, ..
            } => {
                // self.declare(id.lexeme);
                // self.define(id.lexeme);
                let mut encl_func = self.current_func.clone();
                encl_func = FuncType::Method;
                self.begin_scope();
                self.declare("self");
                self.define("self");
                for param in params {
                    self.declare(param.0);
                    self.define(param.0);
                }
                self.resolve_block(body);
                self.end_scope();
                self.current_func = encl_func;
            }
            Stmt::Impl { .. } | Stmt::Break => {}
            _ => unimplemented!(),
        }
    }

    /// resolve a function declaration
    pub fn resolve_function(
        &mut self,
        params: &Vec<(Token<'a>, Token<'a>)>,
        body: &Vec<Stmt<'a>>,
        func_type: FuncType,
    ) {
        let encl_func = self.current_func.clone();
        self.current_func = func_type;

        self.begin_scope();
        for param in params {
            self.declare(param.0.lexeme);
            self.define(param.0.lexeme);
        }
        self.resolve_block(&body);
        self.end_scope();

        self.current_func = encl_func;
    }

    /// resolve an expression
    pub fn resolve_expr(&mut self, expr: &Expr<'a>) {
        match expr {
            Expr::Var { name, id, method } => {
                if let Some((_, es)) = method {
                    for e in es {
                        self.resolve_expr(e);
                    }
                }
                self.resolve_local(*id, name.lexeme);
            }
            Expr::Assign { name, value, id } => {
                self.resolve_expr(value);
                self.resolve_local(*id, name.lexeme);
            }
            Expr::Binary { lhs, rhs, .. } => {
                self.resolve_expr(rhs);
                self.resolve_expr(lhs);
            }
            Expr::Unary { rhs, .. } => self.resolve_expr(rhs),
            Expr::Call { name, args, .. } => {
                self.resolve_expr(name);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::Grouping { expr, .. } => self.resolve_expr(expr),
            _ => {}
        }
    }

    /// resolve a block of statements.
    pub fn resolve_block(&mut self, statements: &[Stmt<'a>]) {
        self.begin_scope();
        for statement in statements {
            self.resolve_stmt(statement);
        }
        self.end_scope();
    }

    /// declare a var in the current scope
    pub fn declare(&mut self, name: &'a str) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                panic!("variable '{}' is already declared in this scope.", name);
            }
            scope.insert(name, VarState::Declared);
        }
    }

    /// define a var in the current scope
    pub fn define(&mut self, name: &'a str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, VarState::Defined);
        }
    }

    /// resolve a vars scope depth
    pub fn resolve_local(&mut self, id: usize, name: &'a str) {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                self.locals.insert(id, depth);
                return;
            }
        }
        // TODO: return default value or error if no var found
    }

    /// start a new scope
    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    /// end the current scope
    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }
}
