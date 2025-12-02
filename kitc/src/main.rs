use clap::{Parser, Subcommand};
use kitlang::codegen::frontend::Compiler;
use std::{fs, path::PathBuf, process::Command};

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
        source: PathBuf,
    },
    /// Compile then run the resulting executable
    Run {
        /// the `.kit` source file
        #[arg(short, long)]
        source: PathBuf,
    },
}

fn main() {
    env_logger::init();
    let args = Cli::parse();

    match args.command {
        Commands::Compile { source } => {
            compile_and_maybe_run(&source, false);
            println!("→ Successfully compiled!");
        }
        Commands::Run { source } => {
            compile_and_maybe_run(&source, true);
        }
    }
}

fn compile_and_maybe_run(source: &PathBuf, run: bool) -> Result<(), ()> {
    let _ = fs::read_to_string(source).unwrap_or_else(|_| panic!("couldn’t read {:?}", source));

    let c_file = source.with_extension("c");

    let mut compiler = Compiler::new(vec![source.clone()], &c_file);

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
    Ok(())
}
