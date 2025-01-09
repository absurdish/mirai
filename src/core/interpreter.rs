use std::cell::RefCell;
use std::rc::Rc;
use crate::core::memory::Memory;
use crate::core::scanner::Value;
use crate::core::parser::Stmt;

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
            Stmt::Print(e) => println!("{:?}", e.eval(Rc::clone(&self.memory))),
            Stmt::Expr(e) => { e.eval(Rc::clone(&self.memory)); }
            Stmt::Var { id, value, .. } => {
                let value = value.eval(Rc::clone(&self.memory));
                self.memory.borrow_mut().set_stack_var(id.lexeme, value);
            }
            Stmt::Block { stmts, .. } => {
                self.memory.borrow_mut().push_stack_frame();
                for stmt in stmts {
                    self.statement(stmt);
                }
                self.memory.borrow_mut().pop_stack_frame();
            }
            _ => unimplemented!()
        }
    }
}