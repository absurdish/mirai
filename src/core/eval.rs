use crate::ast::{
    lexp::{AssignKind, LExpr},
    stmt::Stmt,
    texp::TExpr,
    LitValue::{self, *},
    Token, TokenType,
};
use crate::core::{
    interpreter::Interpreter,
    memory::{Function, Memory, Metadata},
};
use std::cell::RefCell;
use std::rc::Rc;

use super::{env::map, interpreter::RunTimeError};

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
        let mut result = Nil;
        let body_len = func.body.len();

        for (i, stmt) in func.body.iter().enumerate() {
            // check for early return
            if let Some(val) = mem.borrow().specials.get("return") {
                let val = val.clone().unwrap();
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
            let lhs_val = (**lhs).eval(Rc::clone(mem))?;
            let rhs_val = (**rhs).eval(Rc::clone(mem))?;
            return match (op.lexeme, &lhs_val, &rhs_val) {
                ("+", Int { kind: a, owner }, Int { kind: b, .. }) => {
                    Ok(Some(Int { kind: a + b, owner }))
                }
                ("-", Int { kind: a, owner }, Int { kind: b, .. }) => {
                    Ok(Some(Int { kind: a - b, owner }))
                }
                ("*", Int { kind: a, owner }, Int { kind: b, .. }) => {
                    Ok(Some(Int { kind: a * b, owner }))
                }
                ("/", Int { kind: a, owner }, Int { kind: b, .. }) => {
                    Ok(Some(Int { kind: a / b, owner }))
                }
                ("%", Int { kind: a, owner }, Int { kind: b, .. }) => {
                    Ok(Some(Int { kind: a % b, owner }))
                }
                //
                ("+", Unt { kind: a, owner }, Unt { kind: b, .. }) => {
                    Ok(Some(Unt { kind: a + b, owner }))
                }
                ("-", Unt { kind: a, owner }, Unt { kind: b, .. }) => {
                    Ok(Some(Unt { kind: a - b, owner }))
                }
                ("*", Unt { kind: a, owner }, Unt { kind: b, .. }) => {
                    Ok(Some(Unt { kind: a * b, owner }))
                }
                ("/", Unt { kind: a, owner }, Unt { kind: b, .. }) => {
                    Ok(Some(Unt { kind: a / b, owner }))
                }
                ("%", Unt { kind: a, owner }, Unt { kind: b, .. }) => {
                    Ok(Some(Unt { kind: a % b, owner }))
                }
                //
                ("+", Flt { kind: a, owner }, Flt { kind: b, .. }) => {
                    Ok(Some(Flt { kind: a + b, owner }))
                }
                ("-", Flt { kind: a, owner }, Flt { kind: b, .. }) => {
                    Ok(Some(Flt { kind: a - b, owner }))
                }
                ("*", Flt { kind: a, owner }, Flt { kind: b, .. }) => {
                    Ok(Some(Flt { kind: a * b, owner }))
                }
                ("/", Flt { kind: a, owner }, Flt { kind: b, .. }) => {
                    Ok(Some(Flt { kind: a / b, owner }))
                }
                ("%", Flt { kind: a, owner }, Flt { kind: b, .. }) => {
                    Ok(Some(Flt { kind: a % b, owner }))
                }
                _ => Ok(None),
            };
        }
        Ok(None)
    }

    // evaluate expressions
    #[inline]
    #[allow(unused_assignments)]
    pub fn eval(&self, mem: Rc<RefCell<Memory>>) -> Result<LitValue, RunTimeError> {
        match self {
            LExpr::Value { lit, .. } => Ok(lit.clone()),
            LExpr::Vector { elems, .. } => Ok(Vector {
                kind: elems
                    .iter()
                    .map(|f| {
                        let e = f.eval(Rc::clone(&mem));
                        e.expect("failed in vector")
                    })
                    .collect(),
                owner: "",
            }),
            LExpr::Grouping { expr, .. } => expr.eval(mem),
            LExpr::Unary { lhs, op, .. } => self.eval_unary(op, &lhs.eval(mem)?),
            LExpr::Binary { lhs, rhs, op, .. } => {
                self.eval_binary(&lhs.eval(Rc::clone(&mem))?, op, &rhs.eval(mem)?)
            }
            LExpr::Call { name, args, .. } => {
                let name = name.eval(Rc::clone(&mem))?;
                if let LitValue::Fun(func) = name {
                    self.run_fn(*func, args, mem)
                } else if let LitValue::Map(func) = name {
                    let mut litargs = Vec::new();
                    for arg in args {
                        litargs.push(arg.eval(Rc::clone(&mem))?);
                    }
                    let res = if let Ok(e) = map(litargs).get(func).unwrap() {
                        e.clone()
                    } else {
                        LitValue::Nil
                    };
                    Ok(res)
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

                // implement name search here

                if map(vec![LitValue::Nil]).get(name.lexeme).is_some() {
                    return Ok(Map(name.lexeme));
                }

                // if variable can't be found on the stack, return a null value, for now
                return Err(RunTimeError::VariableNotFound(name.lexeme));
            }
            // handle assigning operations
            LExpr::Assign {
                name, lit, kind, ..
            } => {
                // evaluate the value expression
                let eval_value = lit.eval(Rc::clone(&mem))?;
                // get the mutable reference to the memory
                let mut mem = mem.borrow_mut();
                let mut texpr = TExpr::Literal("nil");
                // check if target variable exists on the stack
                if let Some((LitValue::HeapRef(addr), metadata)) = mem.get_stack_var(name.lexeme) {
                    // get the value of the variable from the heap, by the stack address
                    if let Some(heap_obj) = mem.heap.get(&addr) {
                        let mut heap_value = heap_obj.borrow_mut();

                        texpr = heap_value.texpr.clone();
                        // check if variable is constant
                        if let Metadata::Var { is_const, .. } = metadata {
                            if is_const {
                                return Err(RunTimeError::ImmutableAssignement(name.lexeme));
                            }
                        }

                        // assign a new heap value

                        match kind {
                            AssignKind::Eq => {
                                heap_value.value =
                                    eval_value.clone().type_check(heap_value.texpr.clone())?;
                            }
                            AssignKind::Plus | AssignKind::Minus => {
                                heap_value.value = match (&heap_value.value, &eval_value) {
                                    (
                                        LitValue::Int { kind: k1, owner },
                                        LitValue::Int { kind: k2, .. },
                                    ) => LitValue::Int {
                                        kind: if matches!(kind, AssignKind::Plus) {
                                            k1 + k2
                                        } else {
                                            k1 - k2
                                        },
                                        owner: *owner,
                                    },
                                    (
                                        LitValue::Unt { kind: k1, owner },
                                        LitValue::Unt { kind: k2, .. },
                                    ) => LitValue::Unt {
                                        kind: if matches!(kind, AssignKind::Plus) {
                                            k1 + k2
                                        } else {
                                            k1 - k2
                                        },
                                        owner: *owner,
                                    },
                                    (
                                        LitValue::Flt { kind: k1, owner },
                                        LitValue::Flt { kind: k2, .. },
                                    ) => LitValue::Flt {
                                        kind: if matches!(kind, AssignKind::Plus) {
                                            k1 + k2
                                        } else {
                                            k1 - k2
                                        },
                                        owner: *owner,
                                    },
                                    _ => unimplemented!(),
                                }
                                .type_check(heap_value.texpr.clone())?;
                            }
                            _ => unimplemented!(),
                        }

                        return Ok(eval_value.type_check(heap_value.texpr.clone())?);
                    }
                }
                // if variable can't be found, declare a new dynamically typed variable
                let var_id = mem.allocate_heap(eval_value.clone(), texpr);
                mem.set_stack_var(name.lexeme, LitValue::HeapRef(var_id), Metadata::Null);
                Ok(eval_value)
            }
        }
    }

    /// evaluate unary operation expression
    fn eval_unary(&self, op: &Token, rhs: &LitValue) -> Result<LitValue, RunTimeError> {
        match (op.lexeme, rhs) {
            // `-a`, minus unary operation
            ("-", &Int { kind: a, owner }) => Ok(Int { kind: -a, owner }),
            ("-", &Flt { kind: a, owner }) => Ok(Flt { kind: -a, owner }),
            ("-", &ImInt { kind: a, owner }) => Ok(ImInt { kind: -a, owner }),
            ("-", &ImFlt { kind: a, owner }) => Ok(ImFlt { kind: -a, owner }),
            // `!` negation unary operator
            ("!", &Bool { kind: a, owner }) => Ok(Bool { kind: !a, owner }),
            // TODO: `&`, `*`
            _ => Err(RunTimeError::OperationNotAllowed(rhs.clone(), op.lexeme)),
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
            // Vector + Scalar (append to end)
            (Vector { kind: a, owner }, "+", value @ Int { .. })
            | (Vector { kind: a, owner }, "+", value @ Unt { .. })
            | (Vector { kind: a, owner }, "+", value @ Flt { .. }) => {
                let mut vec = a.clone();
                vec.push(value.clone());
                Ok(Vector { kind: vec, owner })
            }

            // Scalar + Vector (prepend to front)
            (value @ Int { .. }, "+", Vector { kind: b, owner })
            | (value @ Unt { .. }, "+", Vector { kind: b, owner })
            | (value @ Flt { .. }, "+", Vector { kind: b, owner }) => {
                let mut vec = vec![value.clone()];
                vec.extend(b.clone());
                Ok(Vector { kind: vec, owner })
            }

            // Vector - Scalar (append negative)
            (Vector { kind: a, owner }, "-", Int { kind: b, .. }) => {
                let mut vec = a.clone();
                vec.push(Int { kind: -*b, owner });
                Ok(Vector { kind: vec, owner })
            }
            (Vector { kind: a, owner }, "-", Flt { kind: b, .. }) => {
                let mut vec = a.clone();
                vec.push(Flt { kind: -*b, owner });
                Ok(Vector { kind: vec, owner })
            }

            // Scalar - Vector (prepend negative)
            (Int { kind: a, owner }, "-", Vector { kind: b, .. }) => {
                let mut vec = vec![Int { kind: *a, owner }];
                vec.extend(b.clone());
                Ok(Vector { kind: vec, owner })
            }
            (Flt { kind: a, owner }, "-", Vector { kind: b, .. }) => {
                let mut vec = vec![Flt { kind: *a, owner }];
                vec.extend(b.clone());
                Ok(Vector { kind: vec, owner })
            }

            // Vector * Scalar (multiply each element)
            (Vector { kind: a, owner }, "*", scalar @ Int { kind: b, .. }) => {
                let vec = a
                    .iter()
                    .map(|x| match x {
                        Int { kind, .. } => Int {
                            kind: kind * b,
                            owner,
                        },
                        Unt { kind, .. } => Unt {
                            kind: (*kind as i64 * b) as u64,
                            owner,
                        },
                        Flt { kind, .. } => Flt {
                            kind: kind * *b as f64,
                            owner,
                        },
                        _ => x.clone(),
                    })
                    .collect();
                Ok(Vector { kind: vec, owner })
            }
            (Vector { kind: a, owner }, "*", Unt { kind: b, .. }) => {
                let vec = a
                    .iter()
                    .map(|x| match x {
                        Int { kind, .. } => Int {
                            kind: kind * *b as i64,
                            owner,
                        },
                        Unt { kind, .. } => Unt {
                            kind: kind * b,
                            owner,
                        },
                        Flt { kind, .. } => Flt {
                            kind: kind * *b as f64,
                            owner,
                        },
                        _ => x.clone(),
                    })
                    .collect();
                Ok(Vector { kind: vec, owner })
            }
            (Vector { kind: a, owner }, "*", Flt { kind: b, .. }) => {
                let vec = a
                    .iter()
                    .map(|x| match x {
                        Int { kind, .. } => Flt {
                            kind: *kind as f64 * b,
                            owner,
                        },
                        Unt { kind, .. } => Flt {
                            kind: *kind as f64 * b,
                            owner,
                        },
                        Flt { kind, .. } => Flt {
                            kind: kind * b,
                            owner,
                        },
                        _ => x.clone(),
                    })
                    .collect();
                Ok(Vector { kind: vec, owner })
            }

            // Scalar * Vector (multiply each element)
            (scalar @ Int { .. }, "*", Vector { kind: b, owner })
            | (scalar @ Unt { .. }, "*", Vector { kind: b, owner })
            | (scalar @ Flt { .. }, "*", Vector { kind: b, owner }) => LExpr::eval_binary(
                self,
                &lhs,
                &Token {
                    lexeme: "*",
                    token: TokenType::Char('*'),
                    lit: None,
                },
                &scalar,
            ),

            // Vector / Scalar (divide each element)
            (Vector { kind: a, owner }, "/", Int { kind: b, .. }) => {
                if *b == 0 {
                    return Err(RunTimeError::OperationNotAllowed(
                        lhs.clone(),
                        "division by 0",
                    ));
                }
                let vec = a
                    .iter()
                    .map(|x| match x {
                        Int { kind, .. } => Int {
                            kind: kind / b,
                            owner,
                        },
                        Unt { kind, .. } => Unt {
                            kind: *kind / *b as u64,
                            owner,
                        },
                        Flt { kind, .. } => Flt {
                            kind: kind / *b as f64,
                            owner,
                        },
                        _ => x.clone(),
                    })
                    .collect();
                Ok(Vector { kind: vec, owner })
            }
            (Vector { kind: a, owner }, "/", Unt { kind: b, .. }) => {
                if *b == 0 {
                    return Err(RunTimeError::OperationNotAllowed(
                        lhs.clone(),
                        "division by 0",
                    ));
                }
                let vec = a
                    .iter()
                    .map(|x| match x {
                        Int { kind, .. } => Int {
                            kind: kind / *b as i64,
                            owner,
                        },
                        Unt { kind, .. } => Unt {
                            kind: kind / b,
                            owner,
                        },
                        Flt { kind, .. } => Flt {
                            kind: kind / *b as f64,
                            owner,
                        },
                        _ => x.clone(),
                    })
                    .collect();
                Ok(Vector { kind: vec, owner })
            }
            (Vector { kind: a, owner }, "/", Flt { kind: b, .. }) => {
                if *b == 0.0 {
                    return Err(RunTimeError::OperationNotAllowed(
                        lhs.clone(),
                        "division by 0",
                    ));
                }
                let vec = a
                    .iter()
                    .map(|x| match x {
                        Int { kind, .. } => Flt {
                            kind: *kind as f64 / b,
                            owner,
                        },
                        Unt { kind, .. } => Flt {
                            kind: *kind as f64 / b,
                            owner,
                        },
                        Flt { kind, .. } => Flt {
                            kind: kind / b,
                            owner,
                        },
                        _ => x.clone(),
                    })
                    .collect();
                Ok(Vector { kind: vec, owner })
            }

            // Vector + Vector (concatenate)
            (Vector { kind: a, owner }, "+", Vector { kind: b, .. }) => {
                let mut vec = a.clone();
                vec.extend(b.clone());
                Ok(Vector { kind: vec, owner })
            }

            // Vector * Vector (cartesian product multiplication)
            (Vector { kind: a, owner }, "*", Vector { kind: b, .. }) => {
                let mut vec = Vec::new();
                for x in a.iter() {
                    for y in b.iter() {
                        match (x, y) {
                            (Int { kind: k1, .. }, Int { kind: k2, .. }) => vec.push(Int {
                                kind: k1 * k2,
                                owner,
                            }),
                            (Int { kind: k1, .. }, Unt { kind: k2, .. }) => vec.push(Int {
                                kind: k1 * *k2 as i64,
                                owner,
                            }),
                            (Int { kind: k1, .. }, Flt { kind: k2, .. }) => vec.push(Flt {
                                kind: *k1 as f64 * k2,
                                owner,
                            }),
                            (Unt { kind: k1, .. }, Int { kind: k2, .. }) => vec.push(Int {
                                kind: *k1 as i64 * k2,
                                owner,
                            }),
                            (Unt { kind: k1, .. }, Unt { kind: k2, .. }) => vec.push(Unt {
                                kind: k1 * k2,
                                owner,
                            }),
                            (Unt { kind: k1, .. }, Flt { kind: k2, .. }) => vec.push(Flt {
                                kind: *k1 as f64 * k2,
                                owner,
                            }),
                            (Flt { kind: k1, .. }, Int { kind: k2, .. }) => vec.push(Flt {
                                kind: k1 * *k2 as f64,
                                owner,
                            }),
                            (Flt { kind: k1, .. }, Unt { kind: k2, .. }) => vec.push(Flt {
                                kind: k1 * *k2 as f64,
                                owner,
                            }),
                            (Flt { kind: k1, .. }, Flt { kind: k2, .. }) => vec.push(Flt {
                                kind: k1 * k2,
                                owner,
                            }),
                            _ => continue,
                        }
                    }
                }
                Ok(Vector { kind: vec, owner })
            }
            // binary operations of `int`
            (Int { kind: a, owner }, "+", Int { kind: b, .. }) => Ok(Int { kind: a + b, owner }),
            (Int { kind: a, owner }, "-", Int { kind: b, .. }) => Ok(Int { kind: a - b, owner }),
            (Int { kind: a, owner }, "*", Int { kind: b, .. }) => Ok(Int { kind: a * b, owner }),
            (Int { kind: a, owner }, "/", Int { kind: b, .. }) => {
                if *b == 0 {
                    return Err(RunTimeError::OperationNotAllowed(
                        lhs.clone(),
                        "division by 0",
                    ));
                }
                Ok(LitValue::Int { kind: a / b, owner })
            }
            (Int { kind: a, owner }, "%", Int { kind: b, .. }) => Ok(Int { kind: a % b, owner }),
            (Int { kind: a, owner }, ">", Int { kind: b, .. }) => Ok(Bool { kind: a > b, owner }),
            (Int { kind: a, owner }, "<", Int { kind: b, .. }) => Ok(Bool { kind: a < b, owner }),
            (Int { kind: a, owner }, "==", Int { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Int { kind: a, owner }, "!=", Int { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Int { kind: a, owner }, "<=", Int { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Int { kind: a, owner }, ">=", Int { kind: b, .. }) => Ok(Bool {
                kind: a >= b,
                owner,
            }),
            // binary operations for unt
            (Unt { kind: a, owner }, "+", Unt { kind: b, .. }) => Ok(Unt { kind: a + b, owner }),
            (Unt { kind: a, owner }, "-", Unt { kind: b, .. }) => Ok(Unt { kind: a - b, owner }),
            (Unt { kind: a, owner }, "*", Unt { kind: b, .. }) => Ok(Unt { kind: a * b, owner }),
            (Unt { kind: a, owner }, "/", Unt { kind: b, .. }) => {
                if *b == 0 {
                    return Err(RunTimeError::OperationNotAllowed(
                        lhs.clone(),
                        "division by 0",
                    ));
                }
                Ok(LitValue::Unt { kind: a / b, owner })
            }
            (Unt { kind: a, owner }, "%", Unt { kind: b, .. }) => Ok(Unt { kind: a % b, owner }),
            (Unt { kind: a, owner }, ">", Unt { kind: b, .. }) => Ok(Bool { kind: a > b, owner }),
            (Unt { kind: a, owner }, "<", Unt { kind: b, .. }) => Ok(Bool { kind: a < b, owner }),
            (Unt { kind: a, owner }, "==", Unt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Unt { kind: a, owner }, "!=", Unt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Unt { kind: a, owner }, "<=", Unt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Unt { kind: a, owner }, ">=", Unt { kind: b, .. }) => Ok(Bool {
                kind: a >= b,
                owner,
            }),
            // binary operations for flt
            (Flt { kind: a, owner }, "+", Flt { kind: b, .. }) => Ok(Flt { kind: a + b, owner }),
            (Flt { kind: a, owner }, "-", Flt { kind: b, .. }) => Ok(Flt { kind: a - b, owner }),
            (Flt { kind: a, owner }, "*", Flt { kind: b, .. }) => Ok(Flt { kind: a * b, owner }),
            (Flt { kind: a, owner }, "/", Flt { kind: b, .. }) => {
                if *b == 0.0 {
                    return Err(RunTimeError::OperationNotAllowed(
                        lhs.clone(),
                        "division by 0",
                    ));
                }
                Ok(LitValue::Flt { kind: a / b, owner })
            }
            (Flt { kind: a, owner }, "%", Flt { kind: b, .. }) => Ok(Flt { kind: a % b, owner }),
            (Flt { kind: a, owner }, ">", Flt { kind: b, .. }) => Ok(Bool { kind: a > b, owner }),
            (Flt { kind: a, owner }, "<", Flt { kind: b, .. }) => Ok(Bool { kind: a < b, owner }),
            (Flt { kind: a, owner }, "==", Flt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Flt { kind: a, owner }, "!=", Flt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Flt { kind: a, owner }, "<=", Flt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Flt { kind: a, owner }, ">=", Flt { kind: b, .. }) => Ok(Bool {
                kind: a >= b,
                owner,
            }),
            // binary operations for imaginary numbers:
            // binary operations of `int`
            (ImInt { kind: a, owner }, "+", ImInt { kind: b, .. }) => {
                Ok(ImInt { kind: a + b, owner })
            }
            (ImInt { kind: a, owner }, "-", ImInt { kind: b, .. }) => {
                Ok(ImInt { kind: a - b, owner })
            }
            (ImInt { kind: a, owner }, "*", ImInt { kind: b, .. }) => {
                Ok(ImInt { kind: a * b, owner })
            }
            (ImInt { kind: a, owner }, "/", ImInt { kind: b, .. }) => {
                if *b == 0 {
                    return Err(RunTimeError::OperationNotAllowed(
                        lhs.clone(),
                        "division by 0",
                    ));
                }
                Ok(LitValue::ImInt { kind: a / b, owner })
            }
            (ImInt { kind: a, owner }, "%", ImInt { kind: b, .. }) => {
                Ok(ImInt { kind: a % b, owner })
            }
            (ImInt { kind: a, owner }, ">", ImInt { kind: b, .. }) => {
                Ok(Bool { kind: a > b, owner })
            }
            (ImInt { kind: a, owner }, "<", ImInt { kind: b, .. }) => {
                Ok(Bool { kind: a < b, owner })
            }
            (ImInt { kind: a, owner }, "==", ImInt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (ImInt { kind: a, owner }, "!=", ImInt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (ImInt { kind: a, owner }, "<=", ImInt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (ImInt { kind: a, owner }, ">=", ImInt { kind: b, .. }) => Ok(Bool {
                kind: a >= b,
                owner,
            }),
            // binary operations for unt
            (ImUnt { kind: a, owner }, "+", ImUnt { kind: b, .. }) => {
                Ok(ImUnt { kind: a + b, owner })
            }
            (ImUnt { kind: a, owner }, "-", ImUnt { kind: b, .. }) => {
                Ok(ImUnt { kind: a - b, owner })
            }
            (ImUnt { kind: a, owner }, "*", ImUnt { kind: b, .. }) => {
                Ok(ImUnt { kind: a * b, owner })
            }
            (ImUnt { kind: a, owner }, "/", ImUnt { kind: b, .. }) => {
                if *b == 0 {
                    return Err(RunTimeError::OperationNotAllowed(
                        lhs.clone(),
                        "division by 0",
                    ));
                }
                Ok(LitValue::ImUnt { kind: a / b, owner })
            }
            (ImUnt { kind: a, owner }, "%", ImUnt { kind: b, .. }) => {
                Ok(ImUnt { kind: a % b, owner })
            }
            (ImUnt { kind: a, owner }, ">", ImUnt { kind: b, .. }) => {
                Ok(Bool { kind: a > b, owner })
            }
            (ImUnt { kind: a, owner }, "<", ImUnt { kind: b, .. }) => {
                Ok(Bool { kind: a < b, owner })
            }
            (ImUnt { kind: a, owner }, "==", ImUnt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (ImUnt { kind: a, owner }, "!=", ImUnt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (ImUnt { kind: a, owner }, "<=", ImUnt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (ImUnt { kind: a, owner }, ">=", ImUnt { kind: b, .. }) => Ok(Bool {
                kind: a >= b,
                owner,
            }),
            // binary operations for flt
            (ImFlt { kind: a, owner }, "+", ImFlt { kind: b, .. }) => {
                Ok(ImFlt { kind: a + b, owner })
            }
            (ImFlt { kind: a, owner }, "-", ImFlt { kind: b, .. }) => {
                Ok(ImFlt { kind: a - b, owner })
            }
            (ImFlt { kind: a, owner }, "*", ImFlt { kind: b, .. }) => {
                Ok(ImFlt { kind: a * b, owner })
            }
            (ImFlt { kind: a, owner }, "/", ImFlt { kind: b, .. }) => {
                if *b == 0.0 {
                    return Err(RunTimeError::OperationNotAllowed(
                        lhs.clone(),
                        "division by 0",
                    ));
                }
                Ok(LitValue::ImFlt { kind: a / b, owner })
            }
            (ImFlt { kind: a, owner }, "%", ImFlt { kind: b, .. }) => {
                Ok(ImFlt { kind: a % b, owner })
            }
            (ImFlt { kind: a, owner }, ">", ImFlt { kind: b, .. }) => {
                Ok(Bool { kind: a > b, owner })
            }
            (ImFlt { kind: a, owner }, "<", ImFlt { kind: b, .. }) => {
                Ok(Bool { kind: a < b, owner })
            }
            (ImFlt { kind: a, owner }, "==", ImFlt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (ImFlt { kind: a, owner }, "!=", ImFlt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (ImFlt { kind: a, owner }, "<=", ImFlt { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (ImFlt { kind: a, owner }, ">=", ImFlt { kind: b, .. }) => Ok(Bool {
                kind: a >= b,
                owner,
            }),
            // binary operations for bool
            (Bool { kind: a, owner }, ">", Bool { kind: b, .. }) => Ok(Bool { kind: a > b, owner }),
            (Bool { kind: a, owner }, "<", Bool { kind: b, .. }) => Ok(Bool { kind: a < b, owner }),
            (Bool { kind: a, owner }, "==", Bool { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Bool { kind: a, owner }, "!=", Bool { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Bool { kind: a, owner }, "<=", Bool { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Bool { kind: a, owner }, ">=", Bool { kind: b, .. }) => Ok(Bool {
                kind: a >= b,
                owner,
            }),
            // binary operations for str
            (Str { kind: a, owner }, "+", Str { kind: b, .. }) => Ok(Str {
                kind: format!("{}{}", a, b),
                owner,
            }),
            (Str { kind: a, owner }, ">", Str { kind: b, .. }) => Ok(Bool { kind: a > b, owner }),
            (Str { kind: a, owner }, "<", Str { kind: b, .. }) => Ok(Bool { kind: a < b, owner }),
            (Str { kind: a, owner }, "==", Str { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Str { kind: a, owner }, "!=", Str { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Str { kind: a, owner }, "<=", Str { kind: b, .. }) => Ok(Bool {
                kind: a <= b,
                owner,
            }),
            (Str { kind: a, owner }, ">=", Str { kind: b, .. }) => Ok(Bool {
                kind: a >= b,
                owner,
            }),
            // binary operations for nil
            (Nil, "<=", Nil) | (Nil, ">=", Nil) | (Nil, "!=", Nil) | (Nil, "==", Nil) => Ok(Bool {
                kind: true,
                owner: "",
            }),
            (Nil, _, _) | (_, _, Nil) => Ok(Bool {
                kind: false,
                owner: "",
            }),
            _ => Err(RunTimeError::UnimplementedOperation(op.lexeme)),
        }
    }
}
