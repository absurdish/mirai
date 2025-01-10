use std::time::Instant;
// Mirai 0.0.x
use cli::cli;
mod cli;
mod consts;
mod core;

fn main() {
    let start_time = Instant::now();
    cli();
    let elapsed_time = start_time.elapsed();
    println!("time took to execute: {:?}", elapsed_time);
}
