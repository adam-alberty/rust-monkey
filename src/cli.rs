use std::path::PathBuf;

use clap::Parser;

use crate::repl;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path of the script to run
    script: Option<PathBuf>,
}

/// CLI entrypoint
pub fn main() {
    let args = Args::parse();

    match args.script {
        Some(_) => todo!(),
        None => repl::run(),
    }
}
