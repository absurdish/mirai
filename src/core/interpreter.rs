use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use crate::core::memory::{Function, Memory};
use crate::core::scanner::Value;
use crate::core::parser::Stmt;
use crate::core::types::type_check;

#[derive(Debug)]
pub struct Interpreter<'a> {
    // program memory
    pub memory: Rc<RefCell<Memory<'a>>>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self {
            memory: Rc::new(RefCell::new(Memory::new())),
        }
    }

    pub fn new_with_memory(memory: Rc<RefCell<Memory<'a>>>) -> Self {
        Self { memory }
    }

    pub fn start(&mut self, stmts: Vec<Stmt<'a>>) -> Rc<RefCell<Memory<'a>>> {
        for stmt in stmts {
            self.statement(stmt);
        }
        Rc::clone(&self.memory)
    }

    pub fn statement(&mut self, stmt: Stmt<'a>) {
        match stmt {
            Stmt::Print(e) => println!("{}", e.eval(Rc::clone(&self.memory))),
            Stmt::Expr(e) => { e.eval(Rc::clone(&self.memory)); }
            Stmt::Var { id, value, type_, .. } => {
                let value = value.eval(Rc::clone(&self.memory));
                type_check(type_.token_type, &value);
                let var_id = self.memory.borrow_mut().allocate_heap(value);
                self.memory.borrow_mut().set_stack_var(id.lexeme, Value::HeapRef(var_id));
            }
            Stmt::Fn { id, params, body, type_, .. } => {
                self.memory.borrow_mut().add_function(id.lexeme, Function {
                    name: id.lexeme,
                    type_: type_.lexeme,
                    params: params.iter().map(|(a, b)| (a.lexeme, b.lexeme)).collect(),
                    body: Rc::new(body),
                });
            }
            Stmt::Block { stmts, .. } => {
                self.memory.borrow_mut().push_stack_frame();
                for stmt in stmts {
                    self.statement(stmt);
                }
                self.memory.borrow_mut().pop_stack_frame();
            }
            Stmt::If { pred, body, else_b } => {
                let is_truthy = pred.eval(Rc::clone(&self.memory)).is_truthy();
                if is_truthy {
                    self.statement(*body);
                } else if !pred.eval(Rc::clone(&self.memory)).is_truthy() && else_b.is_some() {
                    self.statement(*else_b.unwrap());
                }
            }
            Stmt::While { pred, body } => {
                while pred.eval(Rc::clone(&self.memory)).is_truthy() {
                    if let Stmt::Block { stmts, .. } = body.deref() {
                        for stmt in stmts {
                            self.statement(stmt.clone());
                            if let Stmt::Break = stmt.deref() {
                                return;
                            }
                        }
                    } else {
                        self.statement(*body.clone());
                    }
                }
            }
            Stmt::Break => {}
            _ => unimplemented!()
        }
    }
}