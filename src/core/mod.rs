use crate::ast::Ast;
use crate::core::interpreter::Interpreter;
use crate::core::resolver::Resolver;
use std::process::exit;
pub mod eval;
pub mod interpreter;
pub mod memory;
pub mod resolver;
pub mod types;

pub fn run(input: &'static str) {
    let mut ast = Ast::new(input);
    let stmts = match ast.start() {
        Ok(stmts) => stmts,
        Err(err) => {
            eprintln!("{:?}", err);
            exit(0);
        }
    };

    // handles scopes and locality
    let mut resolver = Resolver::new();
    Resolver::resolve(&mut resolver, &stmts);
    // interpreters the code
    let mut interpreter = Interpreter::new();
    if let Err(e) = interpreter.start(stmts) {
        eprintln!("error: {:?}", e)
    }
    // memory testing code
    //
    // let mem = interpreter.memory.borrow();
    // let heap: HashMap<usize, Value> = mem.heap
    //     .iter()
    //     .map(|(i, f)| (*i, f.borrow().value.clone()))
    //     .collect();
    // println!("stack: {:?}", mem.stack);
    // println!("heap: {:?}", heap);
}
