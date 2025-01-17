#[macro_export]
macro_rules! throw {
    ($msg:expr, $exit:expr) => {
        println!("{}{}", "error: ".red().bold(), $msg.to_string().red());
        if $exit {
            std::process::exit(1);
        }
    };
    ($msg:expr) => {
        println!("{}{}", "error: ".red().bold(), $msg.to_string().red())
    };
}

#[macro_export]
macro_rules! warn {
    ($msg:expr) => {
        println!("{}{}", "warning: ".yellow().bold(), $msg.to_string().yellow());
    };
    ($msg:expr, $status:expr) => {
        if $status {
            println!("{}{}", "warning: ".yellow().bold(), $msg.to_string().yellow());
        }
    };
}