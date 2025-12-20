use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilationError {
    #[error("Failed to compile: {0}")]
    CompileError(String),

    #[error("Failed to parse: {0}")]
    ParseError(String),

    #[error("Failed to compile C code:\n{}", String::from_utf8_lossy(.0))]
    CCompileError(Vec<u8>),

    #[error("Failed to find system C toolchain")]
    ToolchainNotFound,

    #[error("Invalid output path")]
    InvalidOutputPath,

    #[error("Unsupported toolchain: {0}")]
    UnsupportedToolchain(String),

    #[error(transparent)]
    Io(std::io::Error),
}
