use crate::core::memory::Memory;
use crate::core::parser::{Expr, Stmt};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ApplyKind<'a> {
    // normal types like int, str, f64...
    Normal(&'a str),
    // Compound and other kinds will be added in the future
}

#[derive(Clone, Debug, PartialEq)]
pub struct Method<'a> {
    pub name: &'a str,
    pub applies: ApplyKind<'a>,
    pub type_: &'a str,
    pub params: Vec<(&'a str, &'a str)>,
    pub body: Rc<Vec<Stmt<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Env<'a> {
    // linked std paths, files, packages, ...
    linked_mods: HashMap<&'a str, Rc<RefCell<Memory<'a>>>>,
    // (type, [method1, method2, ...])
    methods: HashMap<&'a str, Method<'a>>,
    // used for returns and breaks
    pub specials: HashMap<&'a str, Expr<'a>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Self {
            linked_mods: HashMap::new(),
            methods: HashMap::new(),
            specials: HashMap::new(),
        }
    }

    pub fn define_method(&mut self, name: &'a str, method: Method<'a>) {
        self.methods.insert(name, method);
    }

    pub fn get_method(&mut self, name: &'a str) -> Option<&Method<'a>> {
        self.methods.get(&name)
    }
}
