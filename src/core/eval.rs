use crate::core::interpreter::Interpreter;
use crate::core::memory::{Function, Memory};
use crate::core::parser::{Expr, Stmt};
use crate::core::scanner::{Token, TokenType, Value};
use crate::core::types::type_check;
use std::cell::RefCell;
use std::rc::Rc;

#[allow(dead_code)]
#[derive(Debug)]
struct FunctionOptInfo {
    is_arithmetic: bool,
    single_expression: bool,
    constant_args: bool,
    pure_function: bool,
}

impl<'a> Expr<'a> {
    #[inline]
    pub fn run_fn(
        &self,
        func: Function<'a>,
        args: &[Expr<'a>],
        mem: Rc<RefCell<Memory<'a>>>,
    ) -> Value<'a> {
        if args.len() != func.params.len() {
            return Value::Nil;
        }

        // analyze function for optimization opportunities
        let optimization = self.analyze_function(&func, args);

        // fast path for simple arithmetic functions
        if let Some(result) = self.try_optimize_arithmetic(&optimization, args, &mem) {
            return result;
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
        for (i, (name, type_)) in func.params.iter().enumerate() {
            let value = args[i].eval(Rc::clone(&mem));
            type_check(TokenType::Keyword(type_), &value);
            locals.push((name, value));
        }

        // bulk set variables to avoid multiple borrow_mut calls
        for (name, value) in locals {
            meme.set_stack_var(name, value);
        }

        // use pre-allocated interpreter for the function
        let mut int = Interpreter::new_with_memory(Rc::clone(&mem));

        // fast path for single expression functions
        if func.body.len() == 1 {
            if let Some(stmt) = func.body.first() {
                if let Stmt::Expr(e) = stmt {
                    let result = e.eval(Rc::new(RefCell::new(meme)));
                    type_check(TokenType::Keyword(func.type_), &result);
                    return result;
                }
            }
        }

        // regular execution path with optimizations
        let mut result = Value::Nil;
        let body_len = func.body.len();

        for (i, stmt) in func.body.iter().enumerate() {
            // check for early return
            if let Some(val) = mem.borrow().env.specials.get("return") {
                let value = val.eval(Rc::new(RefCell::new(meme)));
                type_check(TokenType::Keyword(func.type_), &value);
                return value;
            }

            // optimize last expression handling
            if i + 1 == body_len {
                match stmt {
                    Stmt::Expr(e) => {
                        result = e.eval(Rc::new(RefCell::new(meme)));
                        type_check(TokenType::Keyword(func.type_), &result);
                        break;
                    }
                    Stmt::Return(e) => {
                        result = e.eval(Rc::new(RefCell::new(meme)));
                        type_check(TokenType::Keyword(func.type_), &result);
                        return result;
                    }
                    _ => int.statement(stmt.clone()),
                }
            } else {
                match stmt {
                    Stmt::Return(e) => {
                        result = e.eval(Rc::new(RefCell::new(meme)));
                        type_check(TokenType::Keyword(func.type_), &result);
                        return result;
                    }
                    _ => int.statement(stmt.clone()),
                }
            }
        }

        result
    }

    #[inline]
    fn analyze_function(&self, func: &Function<'a>, args: &[Expr<'a>]) -> FunctionOptInfo {
        FunctionOptInfo {
            is_arithmetic: self.is_arithmetic_function(func),
            single_expression: func.body.len() == 1,
            constant_args: self.has_constant_args(args),
            pure_function: self.is_pure_function(func),
        }
    }

    #[inline]
    fn is_arithmetic_function(&self, func: &Function<'a>) -> bool {
        if func.body.len() != 1 {
            return false;
        }

        if let Some(Stmt::Expr(Expr::Binary { .. })) = func.body.first() {
            return true;
        }
        false
    }

    #[inline]
    fn has_constant_args(&self, args: &[Expr<'a>]) -> bool {
        args.iter().all(|arg| matches!(arg, Expr::Value { .. }))
    }

    #[inline]
    fn is_pure_function(&self, func: &Function<'a>) -> bool {
        !func.body.iter().any(|stmt| {
            matches!(
                stmt,
                Stmt::Var { .. } | Stmt::Fn { .. } | Stmt::Method { .. } | Stmt::While { .. }
            )
        })
    }

    #[inline]
    fn try_optimize_arithmetic(
        &self,
        opt: &FunctionOptInfo,
        args: &[Expr<'a>],
        mem: &Rc<RefCell<Memory<'a>>>,
    ) -> Option<Value<'a>> {
        if !opt.is_arithmetic || !opt.constant_args {
            return None;
        }

        if let Some(Expr::Binary { op, lhs, rhs, .. }) = args.first() {
            // Fast path for common arithmetic operations
            let lhs_val = (**lhs).eval(Rc::clone(mem));
            let rhs_val = (**rhs).eval(Rc::clone(mem));

            match (op.lexeme, &lhs_val, &rhs_val) {
                ("+", Value::Int64(a), Value::Int64(b)) => Some(Value::Int64(a + b)),
                ("+", Value::Int(a), Value::Int(b)) => Some(Value::Int(a + b)),
                ("-", Value::Int64(a), Value::Int64(b)) => Some(Value::Int64(a - b)),
                ("-", Value::Int(a), Value::Int(b)) => Some(Value::Int(a - b)),
                ("*", Value::Int64(a), Value::Int64(b)) => Some(Value::Int64(a * b)),
                ("*", Value::Int(a), Value::Int(b)) => Some(Value::Int(a * b)),
                _ => None,
            }
        } else {
            None
        }
    }

    // evaluate expressions
    #[inline]
    pub fn eval(&self, mem: Rc<RefCell<Memory<'a>>>) -> Value<'a> {
        match self {
            Expr::Value { value, .. } => value.clone(),
            Expr::Grouping { expr, .. } => expr.eval(mem),
            Expr::Unary { rhs, op, .. } => self.eval_unary(op, &rhs.eval(mem)),
            Expr::Binary { lhs, rhs, op, .. } => {
                self.eval_binary(&lhs.eval(Rc::clone(&mem)), op, &rhs.eval(mem))
            }
            Expr::Call { name, args, .. } => {
                if let Value::Function(func) = name.eval(Rc::clone(&mem)) {
                    self.run_fn(func, args, mem)
                } else {
                    Value::Nil
                }
            }
            Expr::Var { name, method, .. } => {
                let mem_ref = mem.borrow();
                if let Some(Value::HeapRef(id)) = mem_ref.get_stack_var(name.lexeme) {
                    if let Some(heap_obj) = mem_ref.heap.get(&id) {
                        let value = heap_obj.borrow().value.clone();

                        if let Some((method_name, args)) = method {
                            drop(mem_ref);
                            return self.handle_method(value, method_name, args, mem);
                        }
                        return value;
                    }
                }
                mem_ref.get_stack_var(name.lexeme).unwrap_or(Value::Nil)
            }
            Expr::Assign { name, value, .. } => {
                let eval_value = value.eval(Rc::clone(&mem));
                let mut mem = mem.borrow_mut();

                if let Some(Value::HeapRef(id)) = mem.get_stack_var(name.lexeme) {
                    if let Some(heap_obj) = mem.heap.get(&id) {
                        let mut heap_value = heap_obj.borrow_mut();
                        if heap_value.value.same_type(&eval_value) {
                            heap_value.value = eval_value.clone();
                        }
                    }
                } else {
                    let var_id = mem.allocate_heap(eval_value.clone());
                    mem.set_stack_var(name.lexeme, Value::HeapRef(var_id));
                }
                eval_value
            }
            Expr::Null => Value::Nil,
        }
    }

    #[inline]
    fn handle_method(
        &self,
        value: Value<'a>,
        _: &str,
        args: &[Expr<'a>],
        mem: Rc<RefCell<Memory<'a>>>,
    ) -> Value<'a> {
        let mut memes = mem.borrow_mut();
        let method = match memes.env.get_method(value.get_type()) {
            Some(m) => m.clone(),
            None => return Value::Nil,
        };

        let mut full_args = Vec::with_capacity(args.len() + 1);
        full_args.push(Expr::Value {
            value,
            id: self.extract_id(),
        });
        full_args.extend_from_slice(args);

        self.run_fn(
            Function {
                name: method.name,
                params: method.params,
                body: method.body,
                type_: method.type_,
            },
            &full_args,
            Rc::new(RefCell::new(memes.clone())),
        )
    }

    /// evaluate unary operation expression
    fn eval_unary(&self, op: &Token, rhs: &Value) -> Value<'a> {
        match (op.lexeme, rhs) {
            // `-a`, minus unary operation
            ("-", &Value::Int(a)) => Value::Int(-a),
            ("-", &Value::Int64(a)) => Value::Int64(-a),
            ("-", &Value::Flt(a)) => Value::Flt(-a),
            ("-", &Value::F64(a)) => Value::F64(-a),
            ("-", &Value::Im32(a)) => Value::Im32(-a),
            ("-", &Value::Im64(a)) => Value::Im64(-a),
            ("-", &Value::Bol(a)) => Value::Bol(!a),
            // `++a`, increment unary operation
            ("++", &Value::Int(a)) => Value::Int(a + 1),
            ("++", &Value::Int64(a)) => Value::Int64(a + 1),
            ("++", &Value::Flt(a)) => Value::Flt(a + 1.0),
            ("++", &Value::F64(a)) => Value::F64(a + 1.0),
            ("++", &Value::Im32(a)) => Value::Im32(a + 1.0),
            ("++", &Value::Im64(a)) => Value::Im64(a + 1.0),
            ("++", &Value::Unt(a)) => Value::Unt(a + 1),
            ("++", &Value::Unt64(a)) => Value::Unt64(a + 1),
            // `--a`, decrement unary operation
            ("--", &Value::Int(a)) => Value::Int(a - 1),
            ("--", &Value::Int64(a)) => Value::Int64(a - 1),
            ("--", &Value::Flt(a)) => Value::Flt(a - 1.0),
            ("--", &Value::F64(a)) => Value::F64(a - 1.0),
            ("--", &Value::Im32(a)) => Value::Im32(-a - 1.0),
            ("--", &Value::Im64(a)) => Value::Im64(a - 1.0),
            ("--", &Value::Unt(a)) => {
                if a != 0 {
                    panic!("unsigned integers cannot be negative!")
                }
                Value::Unt(a - 1)
            }
            ("--", &Value::Unt64(a)) => {
                if a != 0 {
                    panic!("unsigned integers cannot be negative!")
                }
                Value::Unt64(a - 1)
            }
            //
            _ => unimplemented!(),
        }
    }
    /// evaluate binary operation expression
    fn eval_binary(&self, lhs: &Value, op: &Token, rhs: &Value) -> Value<'a> {
        match (lhs, op.lexeme, rhs) {
            // binary operations of `int`
            (Value::Int(a), "+", Value::Int(b)) => Value::Int(a + b),
            (Value::Int(a), "-", Value::Int(b)) => Value::Int(a - b),
            (Value::Int(a), "*", Value::Int(b)) => Value::Int(a * b),
            (Value::Int(a), "/", Value::Int(b)) => {
                if *b == 0 {
                    panic!("division by zero")
                }
                Value::Int(a / b)
            }
            (Value::Int(a), "%", Value::Int(b)) => Value::Int(a % b),
            (Value::Int(a), ">", Value::Int(b)) => Value::Bol(a > b),
            (Value::Int(a), "<", Value::Int(b)) => Value::Bol(a < b),
            (Value::Int(a), "==", Value::Int(b)) => Value::Bol(a == b),
            (Value::Int(a), "!=", Value::Int(b)) => Value::Bol(a != b),
            (Value::Int(a), "<=", Value::Int(b)) => Value::Bol(a <= b),
            (Value::Int(a), ">=", Value::Int(b)) => Value::Bol(a >= b),
            // binary operations of `i32`
            (Value::Int64(a), "+", Value::Int64(b)) => Value::Int64(a + b),
            (Value::Int64(a), "-", Value::Int64(b)) => Value::Int64(a - b),
            (Value::Int64(a), "*", Value::Int64(b)) => Value::Int64(a * b),
            (Value::Int64(a), "/", Value::Int64(b)) => {
                if *b == 0 {
                    panic!("division by zero")
                }
                Value::Int64(a / b)
            }
            (Value::Int64(a), "%", Value::Int64(b)) => Value::Int64(a % b),
            (Value::Int64(a), ">", Value::Int64(b)) => Value::Bol(a > b),
            (Value::Int64(a), "<", Value::Int64(b)) => Value::Bol(a < b),
            (Value::Int64(a), "==", Value::Int64(b)) => Value::Bol(a == b),
            (Value::Int64(a), "!=", Value::Int64(b)) => Value::Bol(a != b),
            (Value::Int64(a), "<=", Value::Int64(b)) => Value::Bol(a <= b),
            (Value::Int64(a), ">=", Value::Int64(b)) => Value::Bol(a >= b),
            //
            (Value::Int64(a), "+", Value::Int(b)) => Value::Int64(a + *b as i64),
            (Value::Int64(a), "-", Value::Int(b)) => Value::Int64(a - *b as i64),
            (Value::Int64(a), "*", Value::Int(b)) => Value::Int64(a * *b as i64),
            (Value::Int64(a), "/", Value::Int(b)) => {
                if *b == 0 {
                    panic!("division by zero")
                }
                Value::Int64(a / *b as i64)
            }
            (Value::Int64(a), "%", Value::Int(b)) => Value::Int64(a % *b as i64),
            (Value::Int64(a), ">", Value::Int(b)) => Value::Bol(*a > *b as i64),
            (Value::Int64(a), "<", Value::Int(b)) => Value::Bol(*a < *b as i64),
            (Value::Int64(a), "==", Value::Int(b)) => Value::Bol(*a == *b as i64),
            (Value::Int64(a), "!=", Value::Int(b)) => Value::Bol(*a != *b as i64),
            (Value::Int64(a), "<=", Value::Int(b)) => Value::Bol(*a <= *b as i64),
            (Value::Int64(a), ">=", Value::Int(b)) => Value::Bol(*a >= *b as i64),
            // binary operations of `unt`
            (Value::Unt(a), "+", Value::Unt(b)) => Value::Unt(a + b),
            (Value::Unt(a), "-", Value::Unt(b)) => Value::Unt(a - b),
            (Value::Unt(a), "*", Value::Unt(b)) => Value::Unt(a * b),
            (Value::Unt(a), "/", Value::Unt(b)) => {
                if *b == 0 {
                    panic!("division by zero")
                }
                Value::Unt(a / b)
            }
            (Value::Unt(a), "%", Value::Unt(b)) => Value::Unt(a % b),
            (Value::Unt(a), ">", Value::Unt(b)) => Value::Bol(a > b),
            (Value::Unt(a), "<", Value::Unt(b)) => Value::Bol(a < b),
            (Value::Unt(a), "==", Value::Unt(b)) => Value::Bol(a == b),
            (Value::Unt(a), "!=", Value::Unt(b)) => Value::Bol(a != b),
            (Value::Unt(a), "<=", Value::Unt(b)) => Value::Bol(a <= b),
            (Value::Unt(a), ">=", Value::Unt(b)) => Value::Bol(a >= b),
            // binary operations of `u32`
            (Value::Unt64(a), "+", Value::Unt64(b)) => Value::Unt64(a + b),
            (Value::Unt64(a), "-", Value::Unt64(b)) => Value::Unt64(a - b),
            (Value::Unt64(a), "*", Value::Unt64(b)) => Value::Unt64(a * b),
            (Value::Unt64(a), "/", Value::Unt64(b)) => {
                if *b == 0 {
                    panic!("division by zero")
                }
                Value::Unt64(a / b)
            }
            (Value::Unt64(a), "%", Value::Unt64(b)) => Value::Unt64(a % b),
            (Value::Unt64(a), ">", Value::Unt64(b)) => Value::Bol(a > b),
            (Value::Unt64(a), "<", Value::Unt64(b)) => Value::Bol(a < b),
            (Value::Unt64(a), "==", Value::Unt64(b)) => Value::Bol(a == b),
            (Value::Unt64(a), "!=", Value::Unt64(b)) => Value::Bol(a != b),
            (Value::Unt64(a), "<=", Value::Unt64(b)) => Value::Bol(a <= b),
            (Value::Unt64(a), ">=", Value::Unt64(b)) => Value::Bol(a >= b),
            // binary operations of `flt`
            (Value::Flt(a), "+", Value::Flt(b)) => Value::Flt(a + b),
            (Value::Flt(a), "-", Value::Flt(b)) => Value::Flt(a - b),
            (Value::Flt(a), "*", Value::Flt(b)) => Value::Flt(a * b),
            (Value::Flt(a), "/", Value::Flt(b)) => {
                if *b == 0.0 {
                    panic!("division by zero")
                }
                Value::Flt(a / b)
            }
            (Value::Flt(a), "%", Value::Flt(b)) => Value::Flt(a % b),
            (Value::Flt(a), ">", Value::Flt(b)) => Value::Bol(a > b),
            (Value::Flt(a), "<", Value::Flt(b)) => Value::Bol(a < b),
            (Value::Flt(a), "==", Value::Flt(b)) => Value::Bol(a == b),
            (Value::Flt(a), "!=", Value::Flt(b)) => Value::Bol(a != b),
            (Value::Flt(a), "<=", Value::Flt(b)) => Value::Bol(a <= b),
            (Value::Flt(a), ">=", Value::Flt(b)) => Value::Bol(a >= b),
            // binary operations of `f64`
            (Value::F64(a), "+", Value::F64(b)) => Value::F64(a + b),
            (Value::F64(a), "-", Value::F64(b)) => Value::F64(a - b),
            (Value::F64(a), "*", Value::F64(b)) => Value::F64(a * b),
            (Value::F64(a), "/", Value::F64(b)) => {
                if *b == 0.0 {
                    panic!("division by zero")
                }
                Value::F64(a / b)
            }
            (Value::F64(a), "%", Value::F64(b)) => Value::F64(a % b),
            (Value::F64(a), ">", Value::F64(b)) => Value::Bol(a > b),
            (Value::F64(a), "<", Value::F64(b)) => Value::Bol(a < b),
            (Value::F64(a), "==", Value::F64(b)) => Value::Bol(a == b),
            (Value::F64(a), "!=", Value::F64(b)) => Value::Bol(a != b),
            (Value::F64(a), "<=", Value::F64(b)) => Value::Bol(a <= b),
            (Value::F64(a), ">=", Value::F64(b)) => Value::Bol(a >= b),
            // binary operations if `im`
            (Value::Im32(a), "+", Value::Im32(b)) => Value::Im32(a + b),
            (Value::Im32(a), "-", Value::Im32(b)) => Value::Im32(a - b),
            (Value::Im32(a), "*", Value::Im32(b)) => Value::Flt(-a * b),
            (Value::Im32(a), "/", Value::Im32(b)) => {
                if *b == 0.0 {
                    panic!("division by zero")
                }
                Value::Im32(a / b)
            }
            (Value::Im32(a), "%", Value::Im32(b)) => Value::Im32(a % b),
            (Value::Im32(a), ">", Value::Im32(b)) => Value::Bol(a > b),
            (Value::Im32(a), "<", Value::Im32(b)) => Value::Bol(a < b),
            (Value::Im32(a), "!=", Value::Im32(b)) => Value::Bol(a != b),
            (Value::Im32(a), "==", Value::Im32(b)) => Value::Bol(a == b),
            (Value::Im32(a), "<=", Value::Im32(b)) => Value::Bol(a <= b),
            (Value::Im32(a), ">=", Value::Im32(b)) => Value::Bol(a >= b),
            // binary operations if `im64`
            (Value::Im64(a), "+", Value::Im64(b)) => Value::Im64(a + b),
            (Value::Im64(a), "-", Value::Im64(b)) => Value::Im64(a - b),
            (Value::Im64(a), "*", Value::Im64(b)) => Value::Im64(a * b),
            (Value::Im64(a), "/", Value::Im64(b)) => {
                if *b == 0.0 {
                    panic!("division by zero")
                }
                Value::Im64(a / b)
            }
            (Value::Im64(a), "%", Value::Im64(b)) => Value::Im64(a % b),
            (Value::Im64(a), ">", Value::Im64(b)) => Value::Bol(a > b),
            (Value::Im64(a), "<", Value::Im64(b)) => Value::Bol(a < b),
            (Value::Im64(a), "!=", Value::Im64(b)) => Value::Bol(a != b),
            (Value::Im64(a), "==", Value::Im64(b)) => Value::Bol(a == b),
            (Value::Im64(a), "<=", Value::Im64(b)) => Value::Bol(a <= b),
            (Value::Im64(a), ">=", Value::Im64(b)) => Value::Bol(a >= b),
            _ => {
                // println!("{:?} {} {:?} isn't implemented", lhs, op.lexeme, rhs);
                Value::Nil
            }
        }
    }

    // extract id from the expression
    pub fn extract_id(&self) -> usize {
        match self {
            Expr::Value { id, .. }
            | Expr::Call { id, .. }
            | Expr::Assign { id, .. }
            | Expr::Var { id, .. }
            | Expr::Unary { id, .. }
            | Expr::Binary { id, .. }
            | Expr::Grouping { id, .. } => *id,
            _ => unimplemented!(),
        }
    }
}
