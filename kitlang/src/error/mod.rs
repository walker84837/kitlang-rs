use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilationError {
    #[error("Failed to compile: {0}")]
    CompileError(String),

    #[error("Failed to parse: {0}")]
    ParseError(String),

    #[error("Failed to compile C code")]
    CCompileError(Vec<u8>),

    #[error("Failed to find system C toolchain")]
    ToolchainNotFound,

    #[error(transparent)]
    Io(std::io::Error),
}
