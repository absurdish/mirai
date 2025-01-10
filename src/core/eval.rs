use std::cell::RefCell;
use std::process::exit;
use std::rc::Rc;
use crate::core::interpreter::Interpreter;
use crate::core::memory::{Function, Memory};
use crate::core::parser::{Expr, Stmt};
use crate::core::scanner::{Token, TokenType, Value};
use crate::core::types::type_check;

impl<'a> Expr<'a> {
    pub fn run_fn(&self, func: Function<'a>, args: &Vec<Expr<'a>>, mem: Rc<RefCell<Memory<'a>>>) -> Value<'a> {
        if args.len() != func.params.len() {
            eprintln!("argument count does not match parameter count.");
            exit(1);
        }
        let mut meme = RefCell::borrow_mut(&mem).clone();
        meme.push_stack_frame();
        for (i, (name, type_)) in func.params.iter().enumerate() {
            let value = args[i].eval(Rc::clone(&mem));
            type_check(TokenType::Keyword(type_), &value);
            meme.set_stack_var(name, value)
        }

        let mut int = Interpreter::new_with_memory(Rc::clone(&mem));
        for (i, stmt) in func.body.iter().enumerate() {
            if i + 1 == func.body.len() {
                // return this statement
                if let Stmt::Expr(e) = stmt {
                    let value = e.eval(Rc::new(RefCell::new(meme.clone())));
                    type_check(TokenType::Keyword(func.type_), &value);
                    return value;
                } else {
                    eprintln!("function must end with an expression");
                    exit(1);
                }
            }
            int.statement(stmt.clone());
        }

        meme.pop_stack_frame();
        Value::Nil
    }

    // evaluate expressions
    pub fn eval(&self, mem: Rc<RefCell<Memory<'a>>>) -> Value<'a> {
        match self {
            Expr::Value { value, .. } => value.clone(),
            Expr::Grouping { expr, .. } => expr.eval(Rc::clone(&mem)),
            Expr::Unary { rhs, op, .. } => self.eval_unary(op, &rhs.eval(Rc::clone(&mem))),
            Expr::Binary { lhs, rhs, op, .. } => self.eval_binary(&lhs.eval(Rc::clone(&mem)), op, &rhs.eval(Rc::clone(&mem))),
            Expr::Null => Value::Nil,
            Expr::Call { name, args, .. } => {
                if let Value::Function(func) = name.eval(Rc::clone(&mem)) {
                    return self.run_fn(func, args, Rc::clone(&mem));
                }
                Value::Nil
            }
            Expr::Var { name, .. } => {
                let mut mem = RefCell::borrow_mut(&mem);
                if let Some(Value::HeapRef(id)) = mem.get_stack_var(name.lexeme) {
                    if let Some(heap_obj) = mem.heap.get(&id) {
                        let heap_value = heap_obj.borrow();
                        return heap_value.value.clone();
                    }
                } else if let Some(val) = mem.get_stack_var(name.lexeme) {
                    return val;
                }
                Value::Nil
            }

            Expr::Assign { name, value, .. } => {
                let value = value.eval(Rc::clone(&mem));
                let mut mem = RefCell::borrow_mut(&mem);

                if let Some(Value::HeapRef(id)) = mem.get_stack_var(name.lexeme) {
                    if let Some(heap_obj) = mem.heap.get(&id) {
                        let mut heap_value = heap_obj.borrow_mut();
                        if !heap_value.value.same_type(&value) {
                            eprintln!("type mismatch!");
                            panic!("Type mismatch encountered!");
                        }
                        heap_value.value = value.clone();
                    }
                } else {
                    let var_id = mem.allocate_heap(value.clone());
                    mem.set_stack_var(name.lexeme, Value::HeapRef(var_id));
                }

                value
            }

            _ => unimplemented!(),
        }
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
                println!("{:?} {} {:?} isn't implemented", lhs, op.lexeme, rhs);
                Value::Nil
            }
        }
    }

    // extract id from the expression
    pub fn extract_id(&self) -> usize {
        match self {
            Expr::Value { id, .. } |
            Expr::Call { id, .. } |
            Expr::Assign { id, .. } |
            Expr::Var { id, .. } |
            Expr::Unary { id, .. } |
            Expr::Binary { id, .. } |
            Expr::Grouping { id, .. } => *id,
            _ => unimplemented!(),
        }
    }

    // extract literal value from the value expression
    pub fn to_lit(&self) -> Option<Value> {
        if let Expr::Value { value, .. } = self {
            Some(value.clone())
        } else {
            None
        }
    }
}