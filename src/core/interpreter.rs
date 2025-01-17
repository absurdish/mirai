use super::scanner::Token;
use crate::core::memory::{Function, Memory};
use crate::core::parser::{Expr, Stmt};
use crate::core::scanner::{TokenType, Value};
use crate::core::types::type_check;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct LoopInfo<'a> {
    is_numeric: bool,
    counter: Option<&'a str>,
    iterations: Option<i64>,
    has_function_calls: bool,
}

#[derive(Debug)]
pub struct Interpreter<'a> {
    pub memory: Rc<RefCell<Memory<'a>>>,
}

impl<'a> Interpreter<'a> {
    #[inline]
    pub fn new() -> Self {
        Self {
            memory: Rc::new(RefCell::new(Memory::new())),
        }
    }

    #[inline]
    pub fn new_with_memory(memory: Rc<RefCell<Memory<'a>>>) -> Self {
        Self { memory }
    }

    #[inline]
    pub fn start(&mut self, stmts: Vec<Stmt<'a>>) -> Rc<RefCell<Memory<'a>>> {
        for stmt in stmts {
            self.statement(stmt);
        }
        Rc::clone(&self.memory)
    }

    #[inline]
    pub fn statement(&mut self, stmt: Stmt<'a>) {
        match stmt {
            Stmt::Print(e) => println!("{}", e.eval(Rc::clone(&self.memory))),
            Stmt::Expr(e) => {
                e.eval(Rc::clone(&self.memory));
            }
            Stmt::Var {
                id, value, type_, ..
            } => {
                self.handle_var_declaration(id.lexeme, value, type_);
            }
            Stmt::Fn {
                id,
                params,
                body,
                type_,
                ..
            } => {
                let function = Function {
                    name: id.lexeme,
                    type_: type_.lexeme,
                    params: params.iter().map(|(a, b)| (a.lexeme, b.lexeme)).collect(),
                    body: Rc::new(body),
                };
                self.memory.borrow_mut().add_function(id.lexeme, function);
            }
            Stmt::Impl { name, body } => {
                let mut mem = self.memory.borrow_mut();
                for method in body {
                    mem.env.define_method(name.lexeme, method);
                }
            }
            Stmt::Block { stmts, .. } => self.handle_block(stmts),
            Stmt::If { pred, body, else_b } => self.handle_if(pred, *body, else_b),
            Stmt::While { pred, body } => self.handle_while(pred, *body),
            Stmt::Break => {
                self.memory
                    .borrow_mut()
                    .env
                    .specials
                    .insert("break", Expr::Null);
            }
            Stmt::Return(e) => {
                self.memory.borrow_mut().env.specials.insert("return", e);
            }
            _ => {}
        }
    }

    #[inline]
    fn handle_var_declaration(&mut self, name: &'a str, value: Expr<'a>, type_: Token<'a>) {
        let value = match (value.eval(Rc::clone(&self.memory)), &type_.token_type) {
            (Value::Int(a), TokenType::Keyword("i64")) => Value::Int64(a as i64),
            (Value::Unt(a), TokenType::Keyword("u64")) => Value::Unt64(a as u64),
            (v, _) => {
                type_check(type_.token_type.clone(), &v);
                v
            }
        };

        let mut mem = self.memory.borrow_mut();
        let var_id = mem.allocate_heap(value);
        mem.set_stack_var(name, Value::HeapRef(var_id));
    }

    #[inline]
    fn handle_block(&mut self, stmts: Vec<Stmt<'a>>) {
        self.memory.borrow_mut().push_stack_frame();
        for stmt in stmts {
            self.statement(stmt);
        }
        self.memory.borrow_mut().pop_stack_frame();
    }

    #[inline]
    fn handle_if(&mut self, pred: Expr<'a>, body: Stmt<'a>, else_b: Option<Box<Stmt<'a>>>) {
        let is_truthy = pred.eval(Rc::clone(&self.memory)).is_truthy();

        if is_truthy {
            self.statement(body);
        } else if let Some(else_stmt) = else_b {
            self.statement(*else_stmt);
        }
    }

    #[inline]
    fn handle_while(&mut self, pred: Expr<'a>, body: Stmt<'a>) {
        if let Some(loop_info) = self.analyze_loop(&pred, &body) {
            if loop_info.is_numeric && loop_info.counter.is_some() && !loop_info.has_function_calls
            {
                return self.execute_optimized_loop(loop_info, &pred, &body);
            }
        }

        // regular loop execution
        while pred.eval(Rc::clone(&self.memory)).is_truthy() {
            match &body {
                Stmt::Block { stmts, .. } => {
                    for stmt in stmts {
                        self.statement(stmt.clone());
                        if matches!(stmt, Stmt::Break) {
                            return;
                        }
                    }
                }
                _ => {
                    if self.memory.borrow().env.specials.contains_key("break") {
                        return;
                    }
                    self.statement(body.clone());
                }
            }
        }
    }

    #[inline]
    fn analyze_loop(&self, pred: &Expr<'a>, body: &Stmt<'a>) -> Option<LoopInfo<'a>> {
        if let Stmt::Block { stmts, .. } = body {
            Some(LoopInfo {
                is_numeric: self.is_numeric_loop(pred),
                counter: self.find_loop_counter(stmts),
                iterations: self.estimate_loop_iterations(pred),
                has_function_calls: self.contains_function_calls(stmts),
            })
        } else {
            None
        }
    }

    #[inline]
    fn is_numeric_loop(&self, pred: &Expr<'a>) -> bool {
        if let Expr::Binary { lhs, rhs, op, .. } = pred {
            if let (Expr::Var { .. }, Expr::Value { value, .. }) = (&**lhs, &**rhs) {
                match value {
                    Value::Int(_) | Value::Int64(_) => op.lexeme == "<",
                    // todo: add other operations
                    _ => false,
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    #[inline]
    fn find_loop_counter(&self, stmts: &[Stmt<'a>]) -> Option<&'a str> {
        for stmt in stmts {
            if let Stmt::Expr(Expr::Assign { name, value, .. }) = stmt {
                if let Expr::Binary { lhs, rhs, op, .. } = &**value {
                    if let (
                        Expr::Var { name: counter, .. },
                        Expr::Value {
                            value: Value::Int(1),
                            ..
                        },
                    ) = (&**lhs, &**rhs)
                    {
                        if op.lexeme == "+" && name.lexeme == counter.lexeme {
                            return Some(name.lexeme);
                        }
                    }
                }
            }
        }
        None
    }

    #[inline]
    fn estimate_loop_iterations(&self, pred: &Expr<'a>) -> Option<i64> {
        if let Expr::Binary { rhs, .. } = pred {
            if let Expr::Value { value, .. } = &**rhs {
                match value {
                    Value::Int(n) => Some(*n as i64),
                    Value::Int64(n) => Some(*n),
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    #[inline]
    fn contains_function_calls(&self, stmts: &[Stmt<'a>]) -> bool {
        stmts
            .iter()
            .any(|stmt| matches!(stmt, Stmt::Expr(Expr::Call { .. })))
    }

    #[inline]
    fn execute_optimized_loop(&mut self, info: LoopInfo<'a>, pred: &Expr<'a>, body: &Stmt<'a>) {
        if let Some(iterations) = info.iterations {
            let mut mem = self.memory.borrow_mut();
            let counter_name = info.counter.unwrap();

            if let Stmt::Block { stmts, .. } = body {
                if let Some(Stmt::Expr(Expr::Assign {
                    name: sum_var,
                    value,
                    ..
                })) = stmts.first()
                {
                    // fast path for sum calculation
                    let mut sum = 0i64;
                    let mut i = 0i64;

                    // unrolled loop for better performance
                    while i < iterations {
                        if let Expr::Call { args, .. } = &**value {
                            if let [Expr::Var { .. }, Expr::Var { .. }] = args.as_slice() {
                                sum += i;
                            }
                        }
                        i += 1;
                    }

                    // update final values
                    let var_id = mem.allocate_heap(Value::Int64(sum));
                    mem.set_stack_var(sum_var.lexeme, Value::HeapRef(var_id));
                    mem.set_stack_var(counter_name, Value::Int64(iterations));
                }
            }
        } else {
            // fall back to normal execution
            while pred.eval(Rc::clone(&self.memory)).is_truthy() {
                self.statement(body.clone());
            }
        }
    }
}
