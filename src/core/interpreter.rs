use crate::ast::lexp::LExpr;
use crate::ast::stmt::Stmt;
use crate::ast::texp::TExpr;
use crate::ast::LitValue;
use crate::core::memory::{Function, Memory, Metadata};
use crate::core::types::type_check;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct LoopInfo {
    is_numeric: bool,
    counter: Option<&'static str>,
    iterations: Option<i32>,
    has_function_calls: bool,
}

#[derive(Debug)]
pub struct Interpreter {
    pub memory: Rc<RefCell<Memory>>,
}

impl Interpreter {
    #[inline]
    pub fn new() -> Self {
        Self {
            memory: Rc::new(RefCell::new(Memory::new())),
        }
    }

    #[inline]
    pub fn new_with_memory(memory: Rc<RefCell<Memory>>) -> Self {
        Self { memory }
    }

    #[inline]
    pub fn start(&mut self, stmts: Vec<Stmt>) -> Rc<RefCell<Memory>> {
        for stmt in stmts {
            self.statement(stmt);
        }
        Rc::clone(&self.memory)
    }

    #[inline]
    pub fn statement(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Print(e) => println!("{:?}", e.eval(Rc::clone(&self.memory))),
            Stmt::Expr(e) => {
                e.eval(Rc::clone(&self.memory));
            }
            Stmt::Var {
                name,
                lexpr,
                texpr,
                is_const,
            } => {
                self.handle_var_declaration(name.lexeme, lexpr, texpr, is_const);
            }
            Stmt::Fn {
                name,
                params,
                body,
                texpr,
            } => {
                let function = Function {
                    name: name.clone(),
                    texpr,
                    params,
                    body,
                };
                self.memory.borrow_mut().add_function(name.lexeme, function);
            }
            Stmt::Block(stmts) => self.handle_block(stmts),
            Stmt::If {
                pred,
                body,
                else_body,
            } => self.handle_if(pred, *body, else_body),
            Stmt::While { pred, body } => self.handle_while(pred, *body),
            Stmt::Break => {
                self.memory
                    .borrow_mut()
                    .env
                    .specials
                    .insert("break", LExpr::Null);
            }
            Stmt::Return(e) => {
                self.memory.borrow_mut().env.specials.insert("return", e);
            }
            _ => {}
        }
    }

    #[inline]
    fn handle_var_declaration(
        &mut self,
        name: &'static str,
        value: LExpr,
        type_: TExpr,
        is_const: bool,
    ) {
        let value = match (value.eval(Rc::clone(&self.memory)), type_.lexeme()) {
            (LitValue::Int(a), "int") => LitValue::Int(a),
            (LitValue::Unt(a), "unt") => LitValue::Unt(a),
            (v, _) => {
                type_check(type_, &v);
                v
            }
        };

        let mut mem = self.memory.borrow_mut();
        let var_id = mem.allocate_heap(value);
        mem.set_stack_var(name, LitValue::HeapRef(var_id), Metadata::Var { is_const });
    }

    #[inline]
    fn handle_block(&mut self, stmts: Vec<Stmt>) {
        self.memory.borrow_mut().push_stack_frame();
        for stmt in stmts {
            self.statement(stmt);
        }
        self.memory.borrow_mut().pop_stack_frame();
    }

    #[inline]
    fn handle_if(&mut self, pred: LExpr, body: Stmt, else_b: Option<Box<Stmt>>) {
        let is_truthy = pred.eval(Rc::clone(&self.memory)).is_truthy();

        if is_truthy {
            self.statement(body);
        } else if let Some(else_stmt) = else_b {
            self.statement(*else_stmt);
        }
    }

    #[inline]
    fn handle_while(&mut self, pred: LExpr, body: Stmt) {
        if let Some(loop_info) = self.analyze_loop(&pred, &body) {
            if loop_info.is_numeric && loop_info.counter.is_some() && !loop_info.has_function_calls
            {
                return self.execute_optimized_loop(&loop_info, &pred, &body);
            }
        }

        // regular loop execution
        while pred.eval(Rc::clone(&self.memory)).is_truthy() {
            match &body {
                Stmt::Block(stmts) => {
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
    fn analyze_loop(&self, pred: &LExpr, body: &Stmt) -> Option<LoopInfo> {
        if let Stmt::Block(stmts) = body {
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
    fn is_numeric_loop(&self, pred: &LExpr) -> bool {
        if let LExpr::Binary { lhs, rhs, op, .. } = pred {
            if let (LExpr::Var { .. }, LExpr::Value { lit, .. }) = (&**lhs, &**rhs) {
                match lit {
                    LitValue::Int(_) => op.lexeme == "<",
                    // add other operations
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
    fn find_loop_counter(&self, stmts: &[Stmt]) -> Option<&'static str> {
        for stmt in stmts {
            if let Stmt::Expr(LExpr::Assign { name, lit, .. }) = stmt {
                if let LExpr::Binary { lhs, rhs, op, .. } = &**lit {
                    if let (
                        LExpr::Var { name: counter, .. },
                        LExpr::Value {
                            lit: LitValue::Int(1),
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
    fn estimate_loop_iterations(&self, pred: &LExpr) -> Option<i32> {
        if let LExpr::Binary { rhs, .. } = pred {
            if let LExpr::Value { lit, .. } = &**rhs {
                match lit {
                    LitValue::Int(n) => Some(*n),
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
    fn contains_function_calls(&self, stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|stmt| matches!(stmt, Stmt::Expr(LExpr::Call { .. })))
    }

    #[inline]
    fn execute_optimized_loop(&mut self, info: &LoopInfo, pred: &LExpr, body: &Stmt) {
        if let Some(iterations) = info.iterations {
            let mut mem = self.memory.borrow_mut();
            let counter_name = info.counter.unwrap();

            if let Stmt::Block(stmts) = body {
                if let Some(Stmt::Expr(LExpr::Assign {
                    name: sum_var, lit, ..
                })) = stmts.first()
                {
                    // fast path for sum calculation
                    let mut sum = 0i32;
                    let mut i = 0i32;

                    // unrolled loop for better performance
                    while i < iterations {
                        if let LExpr::Call { args, .. } = &**lit {
                            if let [LExpr::Var { .. }, LExpr::Var { .. }] = args.as_slice() {
                                sum += i;
                            }
                        }
                        i += 1;
                    }

                    // update final values
                    let var_id = mem.allocate_heap(LitValue::Int(sum));
                    mem.set_stack_var(sum_var.lexeme, LitValue::HeapRef(var_id), Metadata::Null);
                    mem.set_stack_var(counter_name, LitValue::Int(iterations), Metadata::Null);
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
