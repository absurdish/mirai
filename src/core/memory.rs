use smallvec::SmallVec;

use crate::ast::stmt::Stmt;
use crate::ast::texp::TExpr;
use crate::ast::{LitValue, Token};
use crate::core::env::Env;
use std::cell::RefCell;
use std::collections::HashMap;
use std::process::exit;
use std::rc::Rc;

/// represents a function stored in memory.
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: Token,
    pub texpr: TExpr,
    pub body: Vec<Stmt>,
    pub params: SmallVec<[(Token, TExpr); 4]>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct HeapValue {
    pub value: LitValue,
    // owner for borrowing semantics
    owner: Option<Rc<RefCell<Memory>>>,
    borrowed_by: Vec<Rc<RefCell<Memory>>>,
}

#[derive(Clone, Debug)]
pub enum Metadata {
    Var { is_const: bool },
    Null,
}

#[derive(Clone, Debug)]
pub struct Memory {
    // stack frames
    pub stack: Vec<Rc<RefCell<HashMap<&'static str, (LitValue, Metadata)>>>>,
    // heap memory
    pub heap: HashMap<usize, Rc<RefCell<HeapValue>>>,
    // environment
    pub env: Env,
    // next heap id
    next: usize,
}

#[allow(dead_code)]
impl Memory {
    // initializes memory instance
    pub fn new() -> Self {
        Self {
            stack: vec![Rc::new(RefCell::new(HashMap::with_capacity(32)))],
            heap: HashMap::with_capacity(32),
            env: Env::new(),
            next: 0,
        }
    }

    /// pushes a new stack frame for a new scope or function
    #[inline]
    pub fn push_stack_frame(&mut self) {
        self.stack
            .push(Rc::new(RefCell::new(HashMap::with_capacity(16))));
    }

    /// pops the current stack frame, cleaning up local vars

    #[inline]
    pub fn pop_stack_frame(&mut self) {
        if self.stack.len() > 1 {
            self.stack.pop();
        }
    }

    /// sets a var in the current stack frame
    #[inline]
    pub fn set_stack_var(&mut self, name: &'static str, value: LitValue, meta: Metadata) {
        if let Some(frame) = self.stack.last() {
            if let LitValue::HeapRef(id) = &value {
                if let Some(obj) = self.heap.get(id) {
                    if obj.borrow().borrowed_by.is_empty() {
                        frame.borrow_mut().insert(name, (value, meta));
                        return;
                    }
                    return;
                }
            }
            frame.borrow_mut().insert(name, (value, meta));
        }
    }

    /// gets a vars value from the stack
    #[inline]
    pub fn get_stack_var(&self, name: &'static str) -> Option<(LitValue, Metadata)> {
        self.stack
            .iter()
            .rev()
            .find_map(|frame| frame.borrow().get(name).cloned())
    }

    /// Retrieve a function from the stack or heap
    #[inline]
    pub fn get_function(&self, name: &'static str) -> Option<Function> {
        self.get_stack_var(name).and_then(|val| {
            if let LitValue::HeapRef(id) = val.0 {
                self.heap.get(&id).and_then(|obj| {
                    // if let LitValue::Function(func) = &obj.borrow().value {
                    //     Some(func.clone())
                    // } else {
                    //     None
                    // }
                    None
                })
            } else {
                None
            }
        })
    }

    /// allocates a value on the heap and returns a reference ID
    #[inline]
    pub fn allocate_heap(&mut self, value: LitValue) -> usize {
        let id = self.next;
        self.next += 1;
        self.heap.insert(
            id,
            Rc::new(RefCell::new(HeapValue {
                value,
                owner: None,
                borrowed_by: Vec::with_capacity(4),
            })),
        );
        id
    }

    /// borrows a heap object and ensures it can be accessed safely
    pub fn borrow_heap(
        &mut self,
        mem: Rc<RefCell<Memory>>,
        id: usize,
    ) -> Option<Rc<RefCell<HeapValue>>> {
        if let Some(obj) = self.heap.get(&id) {
            obj.borrow_mut().borrowed_by.push(Rc::clone(&mem));
            Some(Rc::clone(obj))
        } else {
            None
        }
    }

    /// returns a heap object, removing the borrowing reference
    pub fn return_heap(&mut self, mem: Rc<RefCell<Memory>>, id: usize) {
        if let Some(obj) = self.heap.get_mut(&id) {
            obj.borrow_mut()
                .borrowed_by
                .retain(|b| !Rc::ptr_eq(b, &mem));
        }
    }

    /// transfers ownership of a heap object to a new memory instance
    pub fn transfer_ownership(&mut self, id: usize, new_owner: Rc<RefCell<Memory>>) {
        if let Some(obj) = self.heap.get_mut(&id) {
            let mut heap_value = obj.borrow_mut();
            if heap_value.borrowed_by.is_empty() {
                heap_value.owner = Some(new_owner); // transfer ownership only if no borrowings
            } else {
                eprintln!("cannot transfer ownership: Heap object is currently borrowed.");
                exit(1);
            }
        }
    }

    /// frees a heap object by id, removing it from the heap
    pub fn free_heap(&mut self, id: usize) {
        self.heap.remove(&id);
    }

    /// adds a function to the memory
    pub fn add_function(&mut self, name: &'static str, function: Function) {
        // let id = self.allocate_heap(Value::Function(function));
        // self.set_stack_var(name, Value::HeapRef(id), Metadata::Null);
    }
}
