use std::{fs::File, path::Path};
use crate::consts::*;
use clap::{error::ErrorKind, CommandFactory, Parser, Subcommand};
use coloredpp::Colorize;
use memmap2::Mmap;

#[derive(Parser)]
#[command(author, version, about, long_about = None, color = clap::ColorChoice::Always)]
struct Cli {
    #[command(subcommand)]
    command: Option<Cmds>,
}

#[derive(Subcommand)]
enum Cmds {
    /// mirai run <targ> [args]
    Run {
        /// The file or directory to run
        targ: String,
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
            Cmds::Run { targ, args } => cmd_run(targ, args),
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
fn cmd_run(targ: String, args: Vec<String>) {
    let valid_extensions = ["mirai", "mir", "mr"];
    let path = Path::new(&targ);

    if let Some(extension) = path.extension().and_then(|ext| ext.to_str()) {
        if !valid_extensions.contains(&extension) {
            println!(
                "{}",
                format!(
                    "error: '{}' has an invalid extension. Allowed: .mirai, .mir, .mr",
                    targ
                )
                .red()
            );
            return;
        }
    } else {
        println!(
            "{}",
            format!("error: '{}' is not a valid file.", targ).red()
        );
        return;
    }
    match File::open(&targ) {
        Ok(file) => match unsafe { Mmap::map(&file) } {
            Ok(mmap) => {
                println!("{}", format!("running: {}", targ).fg_hex(C2));
                let content = std::str::from_utf8(&mmap).unwrap_or("<binary or invalid UTF-8>");
                println!("{}", format!("file content:\n{}", content).fg_hex(C3));
            }
            Err(err) => {
                println!(
                    "{}",
                    format!("error: failed to memory-map file '{}': {}", targ, err).red()
                );
            }
        },
        Err(err) => {
            println!(
                "{}",
                format!("error: failed to read file '{}': {}", targ, err).red()
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
