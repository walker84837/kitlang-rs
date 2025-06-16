use std::path::PathBuf;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompilationError {
    #[error("Unable to read source code: {0}")]
    UnableToReadSource(PathBuf),
    // TODO: add other errors (invalid function, etc)
}
