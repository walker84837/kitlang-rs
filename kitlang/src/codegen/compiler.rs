use std::env;
use std::path::PathBuf;
use which::which;

#[derive(Debug, Clone)]
pub enum Toolchain {
    GCC,
    Clang,
    MSVC,
    Other(String),
}

impl Toolchain {
    /// Return the canonical command to invoke this toolchain.
    pub fn command(&self) -> &str {
        match self {
            Toolchain::GCC => "gcc",
            Toolchain::Clang => "clang",
            Toolchain::MSVC => "cl",
            Toolchain::Other(s) => s,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub toolchain: Toolchain,
    pub target: Vec<PathBuf>,
    pub link_opts: Vec<String>,
    pub compiler_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct CompilerMeta(pub Toolchain, pub PathBuf);

impl CompilerOptions {
    pub fn new(base_meta: CompilerMeta) -> Self {
        Self {
            toolchain: base_meta.0,
            compiler_path: base_meta.1,
            target: Vec::new(),
            link_opts: Vec::new(),
        }
    }

    pub fn link_libs(mut self, libs: &[impl AsRef<str>]) -> Self {
        for lib in libs {
            match self.toolchain {
                Toolchain::GCC | Toolchain::Clang => {
                    self.link_opts.push(format!("-l{}", lib.as_ref()));
                }
                Toolchain::MSVC => {
                    self.link_opts.push(format!("{}.lib", lib.as_ref()));
                }
                Toolchain::Other(_) => {}
            }
        }
        self
    }

    pub fn lib_paths<P>(mut self, paths: &[P]) -> Self
    where
        P: Into<PathBuf> + AsRef<std::ffi::OsStr>,
    {
        for p in paths {
            let path: PathBuf = p.into();
            match self.toolchain {
                Toolchain::GCC | Toolchain::Clang => {
                    self.link_opts.push(format!("-L{}", path.display()));
                }
                Toolchain::MSVC => {
                    self.link_opts.push(format!("/LIBPATH:{}", path.display()));
                }
                Toolchain::Other(_) => {}
            }
        }
        self
    }

    pub fn targets<P>(mut self, targets: &[P]) -> Self
    where
        P: Into<PathBuf> + AsRef<std::ffi::OsStr>,
    {
        for t in targets {
            self.target.push(t.into());
        }
        self
    }

    pub fn build(self) -> CompilerOptions {
        self
    }
}

pub fn get_system_compiler() -> Option<(Toolchain, PathBuf)> {
    // Search from CC environment variables
    if let Ok(env_cc) = env::var("CC") {
        if let Ok(path) = which(&env_cc) {
            return Some((detect_toolchain(&path), path));
        }
    }

    // Search PATH for known compilers
    for name in &["gcc", "clang", "cl", "cc"] {
        if let Ok(path) = which(name) {
            return Some((detect_toolchain(&path), path));
        }
    }

    None
}

fn detect_toolchain(path: &PathBuf) -> Toolchain {
    let exe = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_lowercase();

    if exe.contains("gcc") {
        Toolchain::GCC
    } else if exe.contains("clang") {
        Toolchain::Clang
    } else if exe == "cl" {
        Toolchain::MSVC
    } else {
        Toolchain::Other(exe)
    }
}
