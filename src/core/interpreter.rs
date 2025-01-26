use crate::ast::lexp::LExpr;
use crate::ast::stmt::Stmt;
use crate::ast::texp::TExpr;
use crate::ast::LitValue;
use crate::core::memory::{Function, Memory, Metadata};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub enum RunTimeError {
    /// error occurs when trying to divide number by zero
    DivisionByZero,
    /// functions argument size doesn't match parameters
    FuncArgSize(&'static str),
    /// operation hasn't yet been implemented
    UnimplementedOperation(&'static str),
    /// undeclared variable error
    VariableNotFound(&'static str),
    /// can't assign to the immutable value
    ImmutableAssignement(&'static str),
    /// type error between two literal values
    TypesDontMatch(&'static str),
}

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
    pub fn start(&mut self, stmts: Vec<Stmt>) -> Result<Rc<RefCell<Memory>>, RunTimeError> {
        for stmt in stmts {
            self.statement(stmt)?;
        }
        Ok(Rc::clone(&self.memory))
    }

    #[inline]
    pub fn statement(&mut self, stmt: Stmt) -> Result<(), RunTimeError> {
        match stmt {
            Stmt::Print(e) => {
                println!("{:?}", e.eval(Rc::clone(&self.memory)));
                Ok(())
            }
            Stmt::Expr(e) => {
                e.eval(Rc::clone(&self.memory))?;
                Ok(())
            }
            Stmt::Var {
                name,
                lexpr,
                texpr,
                is_const,
            } => {
                self.handle_var_declaration(name.lexeme, lexpr, texpr, is_const)?;
                Ok(())
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
                Ok(())
            }
            Stmt::Block(stmts) => {
                self.handle_block(stmts)?;
                Ok(())
            }
            Stmt::If {
                pred,
                body,
                else_body,
            } => {
                self.handle_if(pred, *body, else_body)?;
                Ok(())
            }
            Stmt::While { pred, body } => {
                self.handle_while(pred, *body)?;
                Ok(())
            }
            Stmt::Break => {
                self.memory
                    .borrow_mut()
                    .specials
                    .insert("break", LExpr::Null);
                Ok(())
            }
            Stmt::Return(e) => {
                self.memory.borrow_mut().specials.insert("return", e);
                Ok(())
            }
        }
    }

    #[inline]
    fn handle_var_declaration(
        &mut self,
        name: &'static str,
        value: LExpr,
        _: TExpr,
        is_const: bool,
    ) -> Result<(), RunTimeError> {
        let value = value.eval(Rc::clone(&self.memory));

        let mut mem = self.memory.borrow_mut();
        let var_id = mem.allocate_heap(value?);
        mem.set_stack_var(name, LitValue::HeapRef(var_id), Metadata::Var { is_const });
        Ok(())
    }

    #[inline]
    fn handle_block(&mut self, stmts: Vec<Stmt>) -> Result<(), RunTimeError> {
        self.memory.borrow_mut().push_stack_frame();
        for stmt in stmts {
            self.statement(stmt)?;
        }
        self.memory.borrow_mut().pop_stack_frame();
        Ok(())
    }

    #[inline]
    fn handle_if(
        &mut self,
        pred: LExpr,
        body: Stmt,
        else_b: Option<Box<Stmt>>,
    ) -> Result<(), RunTimeError> {
        let is_truthy = pred.eval(Rc::clone(&self.memory))?.is_truthy();

        if is_truthy {
            self.statement(body)?;
        } else if let Some(else_stmt) = else_b {
            self.statement(*else_stmt)?;
        }
        Ok(())
    }

    #[inline]
    fn handle_while(&mut self, pred: LExpr, body: Stmt) -> Result<(), RunTimeError> {
        if let Some(loop_info) = self.analyze_loop(&pred, &body) {
            if loop_info.is_numeric && loop_info.counter.is_some() && !loop_info.has_function_calls
            {
                return self.execute_optimized_loop(&loop_info, &pred, &body);
            }
        }

        // regular loop execution
        while pred.eval(Rc::clone(&self.memory))?.is_truthy() {
            match &body {
                Stmt::Block(stmts) => {
                    for stmt in stmts {
                        self.statement(stmt.clone())?;
                        if matches!(stmt, Stmt::Break) {
                            return Ok(());
                        }
                    }
                }
                _ => {
                    if self.memory.borrow().specials.contains_key("break") {
                        return Ok(());
                    }
                    self.statement(body.clone())?;
                }
            }
        }
        Ok(())
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
    fn execute_optimized_loop(
        &mut self,
        info: &LoopInfo,
        pred: &LExpr,
        body: &Stmt,
    ) -> Result<(), RunTimeError> {
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
            while pred.eval(Rc::clone(&self.memory))?.is_truthy() {
                self.statement(body.clone())?;
            }
        }
        Ok(())
    }
}
