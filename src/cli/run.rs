use crate::cli::utils;
use crate::core::run;
use crate::throw;
use coloredpp::Colorize;
use memmap2::Mmap;
use std::fs::File;
use std::path::Path;
use std::str::from_utf8;
use std::time::Instant;

/// `mirai run <target> [options]`
///
///  - `--dev` // enables dev mode
pub fn cmd_run(target: String, args: Vec<String>) {
    let start_time = Instant::now();
    let valid_extensions = ["mirai", "mir", "mr"];
    let path = Path::new(&target);
    let is_dev = args.contains(&String::from("--dev"));

    // check the file extension
    if let Some(extension) = path.extension().and_then(|ext| ext.to_str()) {
        if !valid_extensions.contains(&extension) {
            throw!(format!("'{}' has an invalid extension, allowed extensions include '.mirai', '.mir' and '.mr'", target));
            // - would you like to rename `target` to `{target.split(".")[1]}.mirai`?
            return;
        }
    } else {
        throw!(format!("'{}' is not a valid file.", target));
        return;
    }

    // open and read the target file
    match File::open(&target) {
        Ok(file) => match unsafe { Mmap::map(&file) } {
            Ok(map) => {
                // print initial text
                print!("{}", "running interpreter".yellow().bold());
                if is_dev {
                    println!("{}{}", " in ".yellow().bold(), "dev mode".red().bold());
                } else {
                    println!();
                }
                // print link to the file
                println!("{}\n", format!("{}:{}:{}", target, 0, 0));

                // convert memory-map to the utf-8 format
                let input = match from_utf8(&map) {
                    Ok(input) => input,
                    Err(_) => {
                        throw!("failed to convert to UTF-8");
                        unreachable!();
                    }
                };
                // run the interpreter with the input
                // - pass the configuration options
                run(input);
                // measure the time of execution (including reading)
                println!(
                    "\n{}{:?}ms",
                    "executed in: ".green(),
                    start_time.elapsed().as_millis()
                );
            }
            Err(err) => throw!(format!("failed to open '{}' (memory-map)", target)),
        },
        Err(err) => throw!(format!("failed to read file '{}': {}", target, err)),
    }
}
