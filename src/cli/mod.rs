use crate::cli::run::cmd_run;
use crate::consts::*;
use crate::throw;
use clap::{error::ErrorKind, CommandFactory, Parser, Subcommand};
use coloredpp::Colorize;

mod run;
mod utils;

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
    // parse the input
    let cli = Cli::parse();

    if let Some(command) = cli.command {
        // handle known commands
        match command {
            Cmds::Run { target, args } => cmd_run(target, args),
            Cmds::Version => cmd_version(),
        }
    } else {
        // handle unknown/invalid commands
        let args: Vec<String> = std::env::args().collect();
        if args.len() > 1 {
            let input_command = &args[1];
            throw!(format!("unknown command '{}'", input_command));
        } else {
            throw!("no command provided. try 'mirai --help' for usage.")
        }
    }
}

/// `mirai version`
fn cmd_version() {
    println!("{}", "Mirai v0.0.0".fg_hex(C1));
}
