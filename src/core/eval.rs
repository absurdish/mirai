use std::cell::RefCell;
use std::rc::Rc;
use crate::core::memory::Memory;
use crate::core::parser::Expr;
use crate::core::scanner::{Token, Value};

impl<'a> Expr<'a> {
    // evaluate expressions
    pub fn eval(&self, mem: Rc<RefCell<Memory<'a>>>) -> Value<'a> {
        match self {
            Expr::Value { value, .. } => value.clone(),
            Expr::Grouping { expr, .. } => expr.eval(Rc::clone(&mem)),
            Expr::Unary { rhs, op, .. } => self.eval_unary(op, &rhs.eval(Rc::clone(&mem))),
            Expr::Binary { lhs, rhs, op, .. } => self.eval_binary(&lhs.eval(Rc::clone(&mem)), op, &rhs.eval(Rc::clone(&mem))),
            Expr::Null => Value::Nil,
            Expr::Var { name, .. } => {
                if let Some(val) = mem.borrow().get_stack_var(name.lexeme) {
                    return val;
                }
                Value::Nil
            }
            _ => unimplemented!(),
        }
    }

    /// evaluate unary operation expression
    fn eval_unary(&self, op: &Token, rhs: &Value) -> Value<'a> {
        match (op.lexeme, rhs) {
            // `-a`, minus unary operation
            ("-", &Value::Int(a)) => Value::Int(-a),
            ("-", &Value::Int32(a)) => Value::Int32(-a),
            ("-", &Value::Flt(a)) => Value::Flt(-a),
            ("-", &Value::F64(a)) => Value::F64(-a),
            ("-", &Value::Im32(a)) => Value::Im32(-a),
            ("-", &Value::Im64(a)) => Value::Im64(-a),
            ("-", &Value::Bol(a)) => Value::Bol(!a),
            // `++a`, increment unary operation
            ("++", &Value::Int(a)) => Value::Int(a + 1),
            ("++", &Value::Int32(a)) => Value::Int32(a + 1),
            ("++", &Value::Flt(a)) => Value::Flt(a + 1.0),
            ("++", &Value::F64(a)) => Value::F64(a + 1.0),
            ("++", &Value::Im32(a)) => Value::Im32(a + 1.0),
            ("++", &Value::Im64(a)) => Value::Im64(a + 1.0),
            ("++", &Value::Unt(a)) => Value::Unt(a + 1),
            ("++", &Value::Unt32(a)) => Value::Unt32(a + 1),
            // `--a`, decrement unary operation
            ("--", &Value::Int(a)) => Value::Int(a - 1),
            ("--", &Value::Int32(a)) => Value::Int32(a - 1),
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
            ("--", &Value::Unt32(a)) => {
                if a != 0 {
                    panic!("unsigned integers cannot be negative!")
                }
                Value::Unt32(a - 1)
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
            // binary operations of `i32`
            (Value::Int32(a), "+", Value::Int32(b)) => Value::Int32(a + b),
            (Value::Int32(a), "-", Value::Int32(b)) => Value::Int32(a - b),
            (Value::Int32(a), "*", Value::Int32(b)) => Value::Int32(a * b),
            (Value::Int32(a), "/", Value::Int32(b)) => {
                if *b == 0 {
                    panic!("division by zero")
                }
                Value::Int32(a / b)
            }
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
            // binary operations of `u32`
            (Value::Unt32(a), "+", Value::Unt32(b)) => Value::Unt32(a + b),
            (Value::Unt32(a), "-", Value::Unt32(b)) => Value::Unt32(a - b),
            (Value::Unt32(a), "*", Value::Unt32(b)) => Value::Unt32(a * b),
            (Value::Unt32(a), "/", Value::Unt32(b)) => {
                if *b == 0 {
                    panic!("division by zero")
                }
                Value::Unt32(a / b)
            }
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
            //
            _ => unimplemented!(),
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