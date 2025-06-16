use clap::{Parser, Subcommand};
use kitlang::codegen::frontend::Compiler;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

#[derive(Parser)]
#[command(name = "kitc", version, about = "kit compiler")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Just compile to C and object
    Compile {
        /// the `.kit` source file
        #[arg(short, long)]
        path: PathBuf,
    },
    /// Compile then run the resulting executable
    Run {
        /// the `.kit` source file
        #[arg(short, long)]
        path: PathBuf,
    },
}

fn main() {
    env_logger::init();
    let args = Cli::parse();

    match args.command {
        Commands::Compile { path } => {
            compile_and_maybe_run(&path, false);
            println!("→ Successfully compiled!");
        }
        Commands::Run { path } => {
            compile_and_maybe_run(&path, true);
        }
    }
}

fn compile_and_maybe_run(source: &PathBuf, run: bool) {
    let _ = fs::read_to_string(source).unwrap_or_else(|_| panic!("couldn’t read {:?}", source));

    // derive the C-file name: e.g. foo.kit → foo.c
    let c_file = source.with_extension("c");

    let mut compiler = Compiler::new(vec![source.clone()], &c_file, None);

    compiler.compile();

    if run {
        let exe = c_file
            .file_stem()
            .and_then(|s| s.to_str())
            .expect("invalid filename")
            .to_string();

        let status = Command::new(format!("./{}", exe))
            .status()
            .expect("failed to launch executable");

        if !status.success() {
            std::process::exit(status.code().unwrap_or(1));
        }
    }
}
