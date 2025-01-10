use crate::{consts::*, core::parser::Parser as MirParser, core::scanner::Scanner};
use clap::{error::ErrorKind, CommandFactory, Parser, Subcommand};
use coloredpp::Colorize;
use memmap2::Mmap;
use std::{fs::File, path::Path};
use std::collections::HashMap;
use std::process::exit;
use crate::core::interpreter::Interpreter;
use crate::core::memory::HeapValue;
use crate::core::resolver::Resolver;
use crate::core::scanner::Value;

mod core;
mod linkers;
mod music;
mod packman;
mod play;

#[derive(Parser)]
#[command(author, version, about, long_about = None, color = clap::ColorChoice::Always)]
struct Cli {
    #[command(subcommand)]
    command: Option<Cmds>,
}

#[derive(Subcommand)]
enum Cmds {
    /// mirai run <target> [args]
    Run {
        /// The file or directory to run
        target: String,
        /// Additional arguments for the program
        #[arg()]
        args: Vec<String>,
    },
    /// mirai version
    Version,
}

pub fn cli() {
    let cli = Cli::parse();

    if let Some(command) = cli.command {
        match command {
            Cmds::Run { target, args } => cmd_run(target, args),
            Cmds::Version => cmd_version(),
        }
    } else {
        let args: Vec<String> = std::env::args().collect();
        if args.len() > 1 {
            let input_command = &args[1];
            let error = Cli::command().error(
                ErrorKind::UnknownArgument,
                format!("unknown command '{}'", input_command),
            );
            println!("{}", error.to_string().red());
        } else {
            println!(
                "{}",
                "no command provided. try 'mirai --help' for usage.".red()
            );
        }
    }
}

/// `mirai run <target> [args]`
fn cmd_run(target: String, args: Vec<String>) {
    let valid_extensions = ["mirai", "mir", "mr"];
    let path = Path::new(&target);

    if let Some(extension) = path.extension().and_then(|ext| ext.to_str()) {
        if !valid_extensions.contains(&extension) {
            println!(
                "{}",
                format!(
                    "error: '{}' has an invalid extension. Allowed: .mirai, .mir, .mr",
                    target
                )
                    .red()
            );
            return;
        }
    } else {
        println!(
            "{}",
            format!("error: '{}' is not a valid file.", target).red()
        );
        return;
    }
    match File::open(&target) {
        Ok(file) => match unsafe { Mmap::map(&file) } {
            Ok(mmap) => {
                println!("{}", format!("running: {}", target).fg_hex(C2));
                let input = std::str::from_utf8(&mmap).unwrap_or("<binary or invalid UTF-8>");
                let mut scanner = Scanner::new(input);
                let tokens = scanner.start();
                let mut parser = MirParser::new(tokens);
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
                // let mem = interpreter.memory.borrow();
                // let heap: HashMap<usize, Value> = mem.heap
                //     .iter()
                //     .map(|(i, f)| (*i, f.borrow().value.clone()))  // Borrowing the value and cloning it
                //     .collect();
                // println!("stack: {:?}", mem.stack);
                // println!("heap: {:?}", heap);
            }
            Err(err) => {
                println!(
                    "{}",
                    format!("error: failed to memory-map file '{}': {}", target, err).red()
                );
            }
        },
        Err(err) => {
            println!(
                "{}",
                format!("error: failed to read file '{}': {}", target, err).red()
            );
        }
    }

    if !args.is_empty() {
        println!("{}", format!("arguments: {:?}", args).fg_hex(C3));
    }
}

/// `mirai version`
fn cmd_version() {
    println!("{}", "Mirai v0.0.0".fg_hex(C1));
}
