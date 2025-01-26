use crate::cli::run::cmd_run;
use crate::consts::*;
use crate::throw;
use coloredpp::Colorize;

mod run;
mod utils;

pub fn cli() {
    let args: Vec<String> = std::env::args().collect();
    
    if args.len() < 2 {
        print_help();
        return;
    }

    match args[1].as_str() {
        "run" => handle_run_command(&args),
        "version" => cmd_version(),
        "--help" | "-h" => print_help(),
        _ => handle_unknown_command(&args),
    }
}

fn handle_run_command(args: &[String]) {
    if args.len() < 3 {
        throw!("No target specified.\n\nUsage: mirai run <target> [args]");
        return;
    }

    let target = args[2].clone();
    let cmd_args = args[3..].to_vec();
    
    cmd_run(&target, cmd_args);
}

fn handle_unknown_command(args: &[String]) {
    if args.len() > 1 {
        throw!(format!("Unknown command '{}'", args[1]));
    } else {
        throw!("No command provided");
    }
    print_help();
}

fn print_help() {
    println!("\n{}", "Mirai Language Toolkit".fg_hex(C1).bold());
    println!("{}", "A safe systems programming language".fg_hex(C1));
    
    println!("\n{}:", "USAGE".bold());
    println!("  mirai [COMMAND] [OPTIONS]");
    
    println!("\n{}:", "COMMANDS".bold());
    println!("  {}      {}", "run".bold(), "Execute a Mirai program");
    println!("  {}  {}", "version".bold(), "Show version information");
    println!("  {}    {}", "help".bold(), "Show this help message");
    
    println!("\n{}:", "OPTIONS".bold());
    println!("  {}  {}", "--dev".bold(), "Enable development mode");
    println!("  {}   {}", "-h, --help".bold(), "Print help");
    
    println!("\n{}:", "EXAMPLES".bold());
    println!("  mirai run main.mirai");
    println!("  mirai run src/ --dev");
    println!("  mirai version");
}

/// `mirai version`
fn cmd_version() {
    println!("{}", "Mirai v0.0.0".fg_hex(C1));
}