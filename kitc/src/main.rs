use clap::{Parser, Subcommand};
use kitlang::codegen::compile;
use kitlang::lexer::Token;
use kitlang::logos::{Lexer, Logos};
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "kitc", version, about = "kit compiler")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Compile {
        #[arg(short, long)]
        path: PathBuf,
    },
    Run {
        #[arg(short, long)]
        path: PathBuf,
    },
}

fn main() {
    let args = Cli::parse();

    match args.command {
        Commands::Run { path } => {
            compile_program(path);
            // run code
        }
        Commands::Compile { path } => {
            compile_program(path);
            println!("code compiled");
        }
    }
}

fn compile_program(source: PathBuf) {
    let source = fs::read_to_string(source).expect("Failed to read source file");

    let mut lexer: Lexer<Token> = Token::lexer(&source);
    compile(&mut lexer);
}
