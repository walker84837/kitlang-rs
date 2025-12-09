use clap::{Parser, Subcommand};
use kitlang::codegen::frontend::Compiler;
use std::{fs, path::PathBuf, process::Command};

type Error = Box<dyn std::error::Error>;

#[derive(Parser)]
#[command(name = "kitc", version, about = "Kit compiler")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile a .kit file to an executable
    Compile {
        /// The `.kit` source file
        source: PathBuf,

        /// The libraries to link against
        #[arg(short, long)]
        libs: Vec<String>,

        /// Compile and immediately run the executable
        #[arg(long)]
        run: bool,
    },
}

fn main() -> Result<(), Error> {
    env_logger::init();
    let Cli { command } = Cli::parse();

    match command {
        Commands::Compile { source, libs, run } => {
            let exe_path = compile(&source, &libs)?;
            if run {
                run_executable(&exe_path)?;
            } else {
                println!("→ Successfully compiled!");
            }
        }
    }
    Ok(())
}

fn compile(source: &PathBuf, libs: &[String]) -> Result<PathBuf, String> {
    fs::read_to_string(source).map_err(|_| format!("couldn't read {:?}", source))?;

    let c_file = source.with_extension("c");
    let exe_path = source.with_extension("");

    let mut compiler = Compiler::new(vec![source.clone()], &c_file, libs.to_vec());

    compiler
        .compile()
        .map_err(|e| format!("failed to compile: {e}"))?;

    // TODO: should this information be moved into a separate struct?
    let compiler_cmd = if cfg!(target_os = "windows") {
        if let Err(e) = which::which("cl") {
            eprintln!("couldn't find MSVC compiler: {e}");
            "gcc"
        } else {
            "cl"
        }
    } else {
        "gcc"
    };

    let mut cmd = Command::new(compiler_cmd);

    let out_flag = if cfg!(target_os = "windows") {
        // TODO: fix this horrible code duplication. This is just a test to see what github actions
        // does
        if let Err(e) = which::which("cl") {
            eprintln!("couldn't find MSVC compiler: {e}");
            "-o"
        } else {
            "/Fo"
        }
    } else {
        "-o"
    };

    cmd.arg(&c_file).arg(out_flag).arg(&exe_path);

    let native_flags = translate_compiler_flags(libs);
    cmd.args(&native_flags);

    let status = cmd
        .status()
        .map_err(|e| format!("failed to launch {}: {}", compiler_cmd, e))?;

    if !status.success() {
        return Err(format!(
            "C compilation failed with {}. Command: {:?}",
            compiler_cmd, cmd
        ));
    }

    Ok(exe_path)
}

fn run_executable(exe_path: &PathBuf) -> Result<(), String> {
    let status = Command::new(exe_path)
        .status()
        .map_err(|e| format!("failed to launch executable: {}", e))?;

    if !status.success() {
        std::process::exit(status.code().unwrap_or(1));
    }
    Ok(())
}

fn translate_compiler_flags(libs: &[String]) -> Vec<String> {
    let mut native_flags = Vec::new();
    for lib_name in libs {
        #[cfg(target_os = "windows")]
        {
            native_flags.push(format!("{}.lib", lib_name));
        }
        #[cfg(not(target_os = "windows"))]
        {
            native_flags.push(format!("-l{}", lib_name));
        }
    }
    native_flags
}
