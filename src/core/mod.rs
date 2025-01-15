use crate::core::interpreter::Interpreter;
use crate::core::parser::Parser;
use crate::core::resolver::Resolver;
use crate::core::scanner::Scanner;
use std::process::exit;
pub mod env;
pub mod eval;
pub mod interpreter;
pub mod memory;
pub mod parser;
pub mod resolver;
pub mod scanner;
pub mod types;

pub fn run(input: &str) {
    // input code tokenizer
    let mut scanner = Scanner::new(input);
    let tokens = scanner.start();
    // token parser
    let mut parser = Parser::new(tokens);
    let stmts = match parser.start() {
        Ok(stmts) => stmts,
        Err(err) => {
            eprintln!("{}", err);
            exit(0)
        }
    };
    // handles scopes and locality
    let mut resolver = Resolver::new();
    Resolver::resolve(&mut resolver, &stmts);
    // interpreters the code
    let mut interpreter = Interpreter::new();
    interpreter.start(stmts);
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
