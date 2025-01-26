use crate::ast::lexp::LExpr;
use crate::ast::stmt::Stmt;
use crate::ast::LitValue::{self, *};
use crate::ast::Token;
use crate::core::interpreter::Interpreter;
use crate::core::memory::{Function, Memory, Metadata};
use std::cell::RefCell;
use std::rc::Rc;

use super::interpreter::RunTimeError;

#[allow(dead_code)]
#[derive(Debug)]
struct FunctionOptInfo {
    is_arithmetic: bool,
    single_expression: bool,
    constant_args: bool,
    pure_function: bool,
}

impl LExpr {
    #[inline]
    pub fn run_fn(
        &self,
        func: Function,
        args: &[LExpr],
        mem: Rc<RefCell<Memory>>,
    ) -> Result<LitValue, RunTimeError> {
        if args.len() != func.params.len() {
            return Err(RunTimeError::FuncArgSize(func.name.lexeme));
        }

        // analyze function for optimization opportunities
        let optimization = self.analyze_function(&func, args);

        // fast path for simple arithmetic functions
        if let Some(result) = self.try_optimize_arithmetic(&optimization, args, &mem)? {
            return Ok(result);
        }

        // regular function execution with optimizations
        let mut meme = unsafe {
            // avoid clone overhead for large memory structures
            std::ptr::read(&*mem.borrow())
        };
        meme.push_stack_frame();

        // pre-allocate space for function locals
        let mut locals = Vec::with_capacity(func.params.len());

        // batch evaluate arguments
        for (i, (name, texpr)) in func.params.iter().enumerate() {
            let value = args[i].eval(Rc::clone(&mem))?.type_check(texpr.clone())?;
            locals.push((name, value));
        }

        // bulk set variables to avoid multiple borrow_mut calls
        for (name, value) in locals {
            meme.set_stack_var(name.lexeme, value, Metadata::Var { is_const: true });
        }

        // use pre-allocated interpreter for the function
        let mut int = Interpreter::new_with_memory(Rc::clone(&mem));

        // fast path for single expression functions
        if func.body.len() == 1 {
            if let Some(stmt) = func.body.first() {
                if let Stmt::Expr(e) = stmt {
                    return e.eval(Rc::new(RefCell::new(meme)))?.type_check(func.texpr);
                }
            }
        }

        // regular execution path with optimizations
        let mut result = LitValue::Nil;
        let body_len = func.body.len();

        for (i, stmt) in func.body.iter().enumerate() {
            // check for early return
            if let Some(val) = mem.borrow().specials.get("return") {
                return val
                    .eval(Rc::new(RefCell::new(meme)))?
                    .type_check(func.texpr);
            }

            // optimize last expression handling
            if i + 1 == body_len {
                match stmt {
                    Stmt::Expr(e) => {
                        result = e
                            .eval(Rc::new(RefCell::new(meme)))?
                            .type_check(func.texpr)?;
                        break;
                    }
                    Stmt::Return(e) => {
                        result = e
                            .eval(Rc::new(RefCell::new(meme)))?
                            .type_check(func.texpr)?;
                        return Ok(result);
                    }
                    _ => int.statement(stmt.clone())?,
                }
            } else {
                match stmt {
                    Stmt::Return(e) => {
                        result = e
                            .eval(Rc::new(RefCell::new(meme)))?
                            .type_check(func.texpr)?;
                        return Ok(result);
                    }
                    _ => int.statement(stmt.clone())?,
                }
            }
        }

        Ok(result)
    }

    #[inline]
    fn analyze_function(&self, func: &Function, args: &[LExpr]) -> FunctionOptInfo {
        FunctionOptInfo {
            is_arithmetic: self.is_arithmetic_function(func),
            single_expression: func.body.len() == 1,
            constant_args: self.has_constant_args(args),
            pure_function: self.is_pure_function(func),
        }
    }

    #[inline]
    fn is_arithmetic_function(&self, func: &Function) -> bool {
        if func.body.len() != 1 {
            return false;
        }

        if let Some(Stmt::Expr(LExpr::Binary { .. })) = func.body.first() {
            return true;
        }
        false
    }

    #[inline]
    fn has_constant_args(&self, args: &[LExpr]) -> bool {
        args.iter().all(|arg| matches!(arg, LExpr::Value { .. }))
    }

    #[inline]
    fn is_pure_function(&self, func: &Function) -> bool {
        !func.body.iter().any(|stmt| {
            matches!(
                stmt,
                Stmt::Var { .. } | Stmt::Fn { .. } | Stmt::While { .. }
            )
        })
    }

    #[inline]
    fn try_optimize_arithmetic(
        &self,
        opt: &FunctionOptInfo,
        args: &[LExpr],
        mem: &Rc<RefCell<Memory>>,
    ) -> Result<Option<LitValue>, RunTimeError> {
        if !opt.is_arithmetic || !opt.constant_args {
            return Ok(None);
        }

        if let Some(LExpr::Binary { op, lhs, rhs, .. }) = args.first() {
            // Fast path for common arithmetic operations
            let lhs_val = (**lhs).eval(Rc::clone(mem));
            let rhs_val = (**rhs).eval(Rc::clone(mem));

            return match (op.lexeme, &lhs_val?, &rhs_val?) {
                ("+", Int(a), Int(b)) => Ok(Some(Int(a + b))),
                ("-", Int(a), Int(b)) => Ok(Some(Int(a - b))),
                ("*", Int(a), Int(b)) => Ok(Some(Int(a * b))),
                ("/", Int(a), Int(b)) => Ok(Some(Int(a / b))),
                ("%", Int(a), Int(b)) => Ok(Some(Int(a % b))),
                _ => Ok(None),
            };
        }
        Ok(None)
    }

    // evaluate expressions
    #[inline]
    pub fn eval(&self, mem: Rc<RefCell<Memory>>) -> Result<LitValue, RunTimeError> {
        match self {
            LExpr::Value { lit, .. } => Ok(lit.clone()),
            LExpr::Grouping { expr, .. } => expr.eval(mem),
            LExpr::Unary { lhs, op, .. } => self.eval_unary(op, &lhs.eval(mem)?),
            LExpr::Binary { lhs, rhs, op, .. } => {
                self.eval_binary(&lhs.eval(Rc::clone(&mem))?, op, &rhs.eval(mem)?)
            }
            LExpr::Call { name, args, .. } => {
                if let LitValue::Fun(func) = name.eval(Rc::clone(&mem))? {
                    self.run_fn(*func, args, mem)
                } else {
                    Ok(LitValue::Nil)
                }
            }
            // handle variable calls
            LExpr::Var { name, .. } => {
                // create a memory reference
                let mem_ref = mem.borrow_mut();
                // check if name exists on the stack
                if let Some((LitValue::HeapRef(addr), _)) = mem_ref.get_stack_var(name.lexeme) {
                    // get the value of the variable from the heap, by the address stored on the stack
                    if let Some(heap_obj) = mem_ref.heap.get(&addr) {
                        // extract value from heap object
                        let value = heap_obj.borrow().value.clone();

                        // if value isn't borrowed, transfer the ownership to the new owner, if called inside the variable

                        // mem_ref.free_heap(addr);
                        // mem_ref.allocate_heap(Value::Nil);

                        // return the value
                        return Ok(value);
                    }

                    return Err(RunTimeError::VariableNotFound(name.lexeme));
                }
                // if variable can't be found on the stack, return a null value, for now
                return Err(RunTimeError::VariableNotFound(name.lexeme));
            }
            // handle assigning operations
            LExpr::Assign { name, lit, .. } => {
                // evaluate the value expression
                let eval_value = lit.eval(Rc::clone(&mem))?;
                // get the mutable reference to the memory
                let mut mem = mem.borrow_mut();

                // check if target variable exists on the stack
                if let Some((LitValue::HeapRef(addr), metadata)) = mem.get_stack_var(name.lexeme) {
                    // get the value of the variable from the heap, by the stack address
                    if let Some(heap_obj) = mem.heap.get(&addr) {
                        let mut heap_value = heap_obj.borrow_mut();

                        // check if variable is constant
                        if let Metadata::Var { is_const, .. } = metadata {
                            if is_const {
                                return Err(RunTimeError::ImmutableAssignement(name.lexeme));
                            }
                        }

                        // if types are checked, assign a new value
                        if heap_value.value.same_type(&eval_value) {
                            // assign a new heap value
                            heap_value.value = eval_value.clone();
                            return Ok(eval_value);
                        }
                        // if not, throw the error
                        return Err(RunTimeError::TypesDontMatch(name.lexeme));
                    }
                }
                // if variable can't be found, declare a new dynamically typed variable
                let var_id = mem.allocate_heap(eval_value.clone());
                mem.set_stack_var(name.lexeme, LitValue::HeapRef(var_id), Metadata::Null);
                Ok(eval_value)
            }
            LExpr::Null => Ok(LitValue::Nil),
        }
    }

    /// evaluate unary operation expression
    fn eval_unary(&self, op: &Token, rhs: &LitValue) -> Result<LitValue, RunTimeError> {
        match (op.lexeme, rhs) {
            // `-a`, minus unary operation
            ("-", &LitValue::Int(a)) => Ok(LitValue::Int(-a)),
            ("-", &LitValue::Flt(a)) => Ok(LitValue::Flt(-a)),
            // TODO
            //
            _ => unimplemented!(),
        }
    }
    /// evaluate binary operation expression
    fn eval_binary(
        &self,
        lhs: &LitValue,
        op: &Token,
        rhs: &LitValue,
    ) -> Result<LitValue, RunTimeError> {
        match (lhs, op.lexeme, rhs) {
            // binary operations of `int`
            (LitValue::Int(a), "+", LitValue::Int(b)) => Ok(LitValue::Int(a + b)),
            (LitValue::Int(a), "-", LitValue::Int(b)) => Ok(LitValue::Int(a - b)),
            (LitValue::Int(a), "*", LitValue::Int(b)) => Ok(LitValue::Int(a * b)),
            (LitValue::Int(a), "/", LitValue::Int(b)) => {
                if *b == 0 {
                    return Err(RunTimeError::DivisionByZero);
                }
                Ok(LitValue::Int(a / b))
            }
            (LitValue::Int(a), "%", LitValue::Int(b)) => Ok(LitValue::Int(a % b)),
            (LitValue::Int(a), ">", LitValue::Int(b)) => Ok(LitValue::Bool(a > b)),
            (LitValue::Int(a), "<", LitValue::Int(b)) => Ok(LitValue::Bool(a < b)),
            (LitValue::Int(a), "==", LitValue::Int(b)) => Ok(LitValue::Bool(a == b)),
            (LitValue::Int(a), "!=", LitValue::Int(b)) => Ok(LitValue::Bool(a != b)),
            (LitValue::Int(a), "<=", LitValue::Int(b)) => Ok(LitValue::Bool(a <= b)),
            (LitValue::Int(a), ">=", LitValue::Int(b)) => Ok(LitValue::Bool(a >= b)),

            _ => Err(RunTimeError::UnimplementedOperation(op.lexeme)),
        }
    }
}
