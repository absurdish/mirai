use std::cell::RefCell;
use std::collections::HashMap;
use std::process::exit;
use std::rc::Rc;
use crate::core::parser::Stmt;
use crate::core::scanner::Value;

/// represents a function stored in memory.
#[derive(Clone, Debug, PartialEq)]
pub struct Function<'a> {
    name: &'a str,
    params: Vec<&'a str>,
    body: Rc<Vec<Stmt<'a>>>,
}


#[derive(Clone, Debug)]
pub struct HeapValue<'a> {
    pub value: Value<'a>,
    // owner for borrowing semantics
    owner: Option<Rc<RefCell<Memory<'a>>>>,
    borrowed_by: Vec<Rc<RefCell<Memory<'a>>>>,
}

#[derive(Clone, Debug)]
pub struct Memory<'a> {
    // stack frames
    pub stack: Vec<Rc<RefCell<HashMap<&'a str, Value<'a>>>>>,
    // heap memory
    pub heap: HashMap<usize, Rc<RefCell<HeapValue<'a>>>>,
    // next heap id
    pub next: usize,
}

impl<'a> Memory<'a> {
    // initializes memory instance
    pub fn new() -> Self {
        Memory {
            stack: vec![Rc::new(RefCell::new(HashMap::new()))], // Global scope
            heap: HashMap::new(),
            next: 0,
        }
    }

    /// pushes a new stack frame for a new scope or function
    pub fn push_stack_frame(&mut self) {
        self.stack.push(Rc::new(RefCell::new(HashMap::new())))
    }

    /// pops the current stack frame, cleaning up local vars
    pub fn pop_stack_frame(&mut self) {
        if self.stack.len() > 1 {
            self.stack.pop();
        } else {
            panic!("cannot pop global scope.");
        }
    }

    /// sets a var in the current stack frame
    pub fn set_stack_var(&mut self, name: &'a str, value: Value<'a>) {
        if let Some(frame) = self.stack.last() {
            if let Value::HeapRef(id) = value {
                if let Some(obj) = self.heap.get(&id) {
                    let heap_value = obj.borrow_mut();
                    if heap_value.borrowed_by.is_empty() {
                        frame.borrow_mut().insert(name, value);
                        return;
                    } else {
                        panic!("Cannot assign value: Heap object is currently borrowed.");
                    }
                }
            }
            frame.borrow_mut().insert(name, value);
        }
    }


    /// gets a vars value from the stack
    pub fn get_stack_var(&self, name: &'a str) -> Option<Value<'a>> {
        for frame in self.stack.iter().rev() {
            if let Some(val) = frame.borrow().get(name) {
                return Some(val.clone());
            }
        }
        None
    }

    /// allocates a val on the heap and returns a ref to it
    pub fn allocate_heap(&mut self, value: Value<'a>) -> usize {
        let id = self.next;
        self.heap.insert(id, Rc::new(RefCell::new(HeapValue { value, owner: None, borrowed_by: vec![] })));
        self.next += 1;
        id
    }

    /// borrows a heap object
    pub fn borrow_heap(&mut self, mem: Rc<RefCell<Memory<'a>>>, id: usize) -> Option<Rc<RefCell<HeapValue<'a>>>> {
        if let Some(obj) = self.heap.get(&id) {
            obj.borrow_mut().borrowed_by.push(Rc::clone(&mem));
            return Some(Rc::clone(obj));
        }
        None
    }

    // returns a heap object
    pub fn return_heap(&mut self, mem: Rc<RefCell<Memory<'a>>>, id: usize) {
        if let Some(obj) = self.heap.get_mut(&id) {
            obj.borrow_mut().borrowed_by.retain(|b| !Rc::ptr_eq(b, &mem));
        }
    }

    // transfers ownership to the new owner
    pub fn transfer_ownership(&mut self, id: usize, new_owner: Rc<RefCell<Memory<'a>>>) {
        if let Some(obj) = self.heap.get_mut(&id) {
            let mut heap_value = obj.borrow_mut();
            if heap_value.borrowed_by.is_empty() {
                heap_value.owner = Some(new_owner);
            } else {
                eprintln!("cannot transfer ownership: Heap object is currently borrowed.");
                exit(1);
            }
        }
    }

    /// transfers ownership to a new memory
    pub fn transfer_heap(&mut self, id: usize, new_owner: Rc<RefCell<Memory<'a>>>) {
        if let Some(obj) = self.heap.get_mut(&id) {
            obj.borrow_mut().owner = Some(new_owner);
        }
    }

    // frees a heap object by id
    pub fn free_heap(&mut self, id: usize) {
        self.heap.remove(&id);
    }

    // adds a function to the memory
    pub fn add_function(&mut self, name: &'a str, function: Function<'a>, to_heap: bool) {
        if to_heap {
            let id = self.allocate_heap(Value::Function(function));
            self.set_stack_var(name, Value::HeapRef(id));
        } else {
            self.set_stack_var(name, Value::Function(function));
        }
    }
}
