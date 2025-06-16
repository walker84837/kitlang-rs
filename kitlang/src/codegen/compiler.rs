use std::path::PathBuf;

enum Toolchain {
    GCC,
    Clang,
    MSVC,
    // other should have some custom settings somewhere
    // but for now add a todo!()
    Other(String),
}

pub struct CompilerOptions {
    pub toolchain: Toolchain,
    // should this be Arc<[T]>?
    pub target: &'static [PathBuf],
    pub link_opts: Vec<String>,
}

fn get_system_compiler() -> Option<String> {
    // TODO: steps to determine system compiler
    // 1. Does $CC exist in the environment? If yes, get the value and check if it exists
    // 2. Does GCC, Clang, or MSVC exist on the system?
    // 3. Does `cc` exist in the current environment?
    // 4. If not, return None
    //
    // Then match on the compiler toolchain

    todo!("Getting system compiler is not implemented yet")
}
