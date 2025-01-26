use crate::ast::lexp::LExpr;
use crate::ast::stmt::Stmt;
use crate::core::memory::Memory;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ApplyKind {
    // normal types like int, str, f64...
    Normal(&'static str),
    // Compound and other kinds will be added in the future
}

#[derive(Clone, Debug, PartialEq)]
pub struct Method {
    pub name: &'static str,
    pub applies: ApplyKind,
    pub type_: &'static str,
    pub params: Vec<(&'static str, &'static str)>,
    pub body: Rc<Vec<Stmt>>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Env {
    // linked std paths, files, packages, ...
    linked_mods: HashMap<&'static str, Rc<RefCell<Memory>>>,
    // (type, [method1, method2, ...])
    methods: HashMap<&'static str, Method>,
    // used for returns and breaks
    pub specials: HashMap<&'static str, LExpr>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            linked_mods: HashMap::new(),
            methods: HashMap::new(),
            specials: HashMap::new(),
        }
    }

    pub fn define_method(&mut self, name: &'static str, method: Method) {
        self.methods.insert(name, method);
    }

    pub fn get_method(&mut self, name: &'static str) -> Option<&Method> {
        self.methods.get(&name)
    }
}
