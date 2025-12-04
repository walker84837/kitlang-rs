use clap::{Parser, Subcommand};
use kitlang::codegen::frontend::Compiler;
use std::{fs, path::PathBuf, process::Command};

type Error = Box<dyn std::error::Error>;

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

impl Commands {
    #[allow(dead_code)]
    pub const fn compiles_code(&self) -> bool {
        matches!(self, Commands::Compile { .. } | Commands::Run { .. })
    }
}

fn main() -> Result<(), Error> {
    env_logger::init();
    let args = Cli::parse();

    let compile = |source: &PathBuf, run: bool| -> Result<(), String> {
        compile_and_maybe_run(&source, run).map_err(|e| format!("failed to compile: {e}"))
    };

    match args.command {
        // TODO: move out compile_and_maybe_run to something else
        Commands::Compile { source } => {
            compile(&source, false)?;
            println!("→ Successfully compiled!");
        }
        Commands::Run { source } => {
            compile(&source, true)?;
        }
    }
    Ok(())
}

fn compile_and_maybe_run(source: &PathBuf, run: bool) -> Result<(), String> {
    fs::read_to_string(source).map_err(|_| format!("couldn't read {:?}", source))?;

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
