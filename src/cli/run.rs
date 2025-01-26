use crate::core::run;
use crate::throw;
use coloredpp::Colorize;
use std::fs;
use std::path::Path;

/// `mirai run <target> [options]`
pub fn cmd_run(target: &str, args: Vec<String>) {
    let valid_extensions = ["mirai", "mir", "mr"];
    let path = Path::new(target);
    let is_dev = args.contains(&String::from("--dev"));

    // Validate file extension
    if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
        if !valid_extensions.contains(&ext) {
            throw!(format!("Invalid extension '{}' for target '{}'", ext, target));
        }
    } else {
        throw!(format!("'{}' has no file extension", target));
    }

    // Read file contents
    let content = match fs::read_to_string(target) {
        Ok(c) => c,
        Err(e) => {
            throw!(format!("Failed to read '{}': {}", target, e));
            return;
        }
    };

    // Convert to 'static str using intentional memory leak
    let static_str: &'static str = Box::leak(content.into_boxed_str());

    // Print execution header
    println!("{}", "running interpreter".yellow().bold());
    if is_dev {
        println!("{} {}", "[DEV MODE]".red().bold(), static_str.len());
    }
    
    // Pass static reference to AST
    run(static_str);
}