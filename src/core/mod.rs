use std::process::exit;
use crate::core::interpreter::Interpreter;
use crate::core::parser::Parser;
use crate::core::resolver::Resolver;
use crate::core::scanner::Scanner;
pub mod parser;
pub mod scanner;
pub mod memory;
pub mod resolver;
pub mod interpreter;
pub mod eval;
pub mod types;
pub mod env;

pub fn run(input: &str) {
    let mut scanner = Scanner::new(input);
    let tokens = scanner.start();
    let mut parser = Parser::new(tokens);
    let stmts = match parser.start() {
        Ok(stmts) => stmts,
        Err(err) => {
            eprintln!("{}", err);
            exit(0)
        }
    };
    let mut resolver = Resolver::new();
    Resolver::resolve(&mut resolver, &stmts);
    let mut interpreter = Interpreter::new();
    interpreter.start(stmts);
    //
    //
    // let mem = interpreter.memory.borrow();
    // let heap: HashMap<usize, Value> = mem.heap
    //     .iter()
    //     .map(|(i, f)| (*i, f.borrow().value.clone()))
    //     .collect();
    // println!("stack: {:?}", mem.stack);
    // println!("heap: {:?}", heap);
}
