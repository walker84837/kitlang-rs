use std::env;
use std::path::PathBuf;
use which::which;

#[derive(Debug)]
pub enum Toolchain {
    GCC,
    Clang,
    MSVC,
    Other(String),
}

#[derive(Debug)]
pub struct CompilerOptions {
    pub toolchain: Toolchain,
    pub target: Vec<PathBuf>,
    pub link_opts: Vec<String>,
    pub compiler_path: PathBuf,
}

pub struct CompilerMeta(Toolchain, PathBuf);

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
                Toolchain::Other(_) => {
                    // optional: warn or ignore
                }
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
                    self.link_opts.push(format!("-L{}", &path.display()));
                }
                Toolchain::MSVC => {
                    self.link_opts.push(format!("/LIBPATH:{}", &path.display()));
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
        for path in targets {
            self.target.push(path.into());
        }
        self
    }

    pub fn build(self) -> CompilerOptions {
        CompilerOptions {
            toolchain: self.toolchain,
            compiler_path: self.compiler_path,
            target: self.target,
            link_opts: self.link_opts,
        }
    }
}

pub fn get_system_compiler() -> Option<(Toolchain, PathBuf)> {
    // check CC environment variable
    if let Ok(env_cc) = env::var("CC")
        && let Ok(path) = which(&env_cc)
    {
        return Some((detect_toolchain(&path), path));
    }

    // check known compilers in order
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

    let toolchain = if exe.contains("gcc") {
        Toolchain::GCC
    } else if exe.contains("clang") {
        Toolchain::Clang
    } else if exe.contains("cl") {
        Toolchain::MSVC
    } else {
        Toolchain::Other(exe)
    };

    toolchain
}
