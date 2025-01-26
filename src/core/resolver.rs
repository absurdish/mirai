use crate::ast::{lexp::LExpr, stmt::Stmt, texp::TExpr, Token};
use smallvec::SmallVec;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum VarState {
    Declared,
    Defined,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FuncType {
    Function,
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
    pub fn resolve(&mut self, stmts: &[Stmt]) -> &HashMap<usize, usize> {
        stmts.iter().for_each(|stmt| self.resolve_stmt(stmt));
        &self.locals
    }

    #[inline]
    pub fn resolve_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Var { name, lexpr, .. } => {
                let name = name.lexeme;
                self.declare(name);
                self.resolve_expr(lexpr);
                self.define(name);
            }
            Stmt::Block(stmts) => {
                self.begin_scope();
                stmts.iter().for_each(|stmt| self.resolve_stmt(stmt));
                self.end_scope();
            }
            Stmt::Expr(expr) => self.resolve_expr(expr),
            Stmt::Fn {
                name, params, body, ..
            } => {
                self.declare(name.lexeme);
                self.define(name.lexeme);
                self.resolve_function(params, body, FuncType::Function);
            }
            Stmt::Print(expr) | Stmt::Return(expr) => self.resolve_expr(expr),
            Stmt::If {
                pred,
                body,
                else_body,
            } => {
                self.resolve_expr(pred);
                self.begin_scope();
                self.resolve_stmt(body);
                self.end_scope();
                if let Some(else_b) = else_body {
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
            _ => {}
        }
    }

    #[inline]
    pub fn resolve_function(
        &mut self,
        params: &SmallVec<[(Token, TExpr); 4]>,
        body: &[Stmt],
        func_type: FuncType,
    ) {
        let encl_func = self.current_func;
        self.current_func = func_type;
        self.begin_scope();

        params.iter().for_each(|(param, _)| {
            self.declare(param.lexeme);
            self.define(param.lexeme);
        });

        self.begin_scope();
        body.iter().for_each(|stmt| self.resolve_stmt(stmt));
        self.end_scope();
        self.end_scope();

        self.current_func = encl_func;
    }

    #[inline]
    pub fn resolve_expr(&mut self, expr: &LExpr) {
        match expr {
            LExpr::Var { name, id, .. } => {
                self.resolve_local(*id, name.lexeme);
            }
            LExpr::Assign { name, id, lit, .. } => {
                self.resolve_expr(lit);
                self.resolve_local(*id, name.lexeme);
            }
            LExpr::Binary { lhs, rhs, .. } => {
                self.resolve_expr(rhs);
                self.resolve_expr(lhs);
            }
            LExpr::Unary { lhs, .. } => self.resolve_expr(lhs),
            LExpr::Call { name, args, .. } => {
                self.resolve_expr(name);
                args.iter().for_each(|arg| self.resolve_expr(arg));
            }
            LExpr::Grouping { expr, .. } => self.resolve_expr(expr),
            _ => {}
        }
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
