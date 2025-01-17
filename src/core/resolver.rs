use crate::core::parser::{Expr, Stmt};
use crate::core::scanner::Token;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum VarState {
    Declared,
    Defined,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FuncType {
    Function,
    Method,
    None,
}

#[derive(Debug)]
pub struct Resolver<'a> {
    scopes: Vec<HashMap<&'a str, VarState>>,
    locals: HashMap<usize, usize>,
    current_func: FuncType,
}

impl<'a> Resolver<'a> {
    #[inline]
    pub fn new() -> Self {
        Self {
            scopes: Vec::with_capacity(8),
            locals: HashMap::with_capacity(32),
            current_func: FuncType::None,
        }
    }

    #[inline]
    pub fn resolve(&mut self, stmts: &[Stmt<'a>]) -> &HashMap<usize, usize> {
        stmts.iter().for_each(|stmt| self.resolve_stmt(stmt));
        &self.locals
    }

    #[inline]
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
                self.declare(id.lexeme);
                self.define(id.lexeme);
                self.resolve_function(params, body, FuncType::Function);
            }
            Stmt::Print(expr) | Stmt::Return(expr) => self.resolve_expr(expr),
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
            Stmt::Method { params, body, .. } => {
                let encl_func = self.current_func;
                self.current_func = FuncType::Method;
                self.begin_scope();
                self.declare("self");
                self.define("self");
                params.iter().for_each(|(name, _)| {
                    self.declare(name);
                    self.define(name);
                });
                self.resolve_block(body);
                self.end_scope();
                self.current_func = encl_func;
            }
            Stmt::Impl { .. } | Stmt::Break => {}
            _ => {}
        }
    }

    #[inline]
    pub fn resolve_function(
        &mut self,
        params: &[(Token<'a>, Token<'a>)],
        body: &[Stmt<'a>],
        func_type: FuncType,
    ) {
        let encl_func = self.current_func;
        self.current_func = func_type;
        self.begin_scope();

        params.iter().for_each(|(param, _)| {
            self.declare(param.lexeme);
            self.define(param.lexeme);
        });

        self.resolve_block(body);
        self.end_scope();
        self.current_func = encl_func;
    }

    #[inline]
    pub fn resolve_expr(&mut self, expr: &Expr<'a>) {
        match expr {
            Expr::Var { name, id, method } => {
                if let Some((_, es)) = method {
                    es.iter().for_each(|e| self.resolve_expr(e));
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
                args.iter().for_each(|arg| self.resolve_expr(arg));
            }
            Expr::Grouping { expr, .. } => self.resolve_expr(expr),
            _ => {}
        }
    }

    #[inline]
    pub fn resolve_block(&mut self, statements: &[Stmt<'a>]) {
        self.begin_scope();
        statements.iter().for_each(|stmt| self.resolve_stmt(stmt));
        self.end_scope();
    }

    #[inline]
    pub fn declare(&mut self, name: &'a str) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                panic!("variable '{}' is already declared in this scope.", name);
            }
            scope.insert(name, VarState::Declared);
        }
    }

    #[inline]
    pub fn define(&mut self, name: &'a str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, VarState::Defined);
        }
    }

    #[inline]
    pub fn resolve_local(&mut self, id: usize, name: &'a str) {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                self.locals.insert(id, depth);
                return;
            }
        }
    }

    #[inline]
    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::with_capacity(8));
    }

    #[inline]
    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }
}
