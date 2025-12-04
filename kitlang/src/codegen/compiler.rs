use std::convert::Infallible;
use std::env;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use which::which;

type NoSearch = fn(&Path) -> Option<String>;

#[derive(Debug, Clone)]
pub enum Toolchain {
    Gcc,
    Clang,
    // TODO: this should be #[cfg(windows)]
    Msvc,
    Other(String),
}

impl FromStr for Toolchain {
    type Err = Infallible;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Ok(match value {
            "gcc" => Toolchain::Gcc,
            "clang" => Toolchain::Clang,
            "cl" => Toolchain::Msvc,
            _ => Toolchain::Other(value.to_string()),
        })
    }
}

pub fn get_lowercase_exe(path: &Path) -> Option<String> {
    Some(path.file_stem().and_then(|s| s.to_str())?.to_lowercase())
}

/// Detect the toolchain for a given executable path.
///
/// If a custom search function `search_fn` is supplied, its result (if any)
/// overrides the simple filename‑based detection.
fn detect_toolchain<SearchFn>(path: &Path, search_fn: Option<SearchFn>) -> Toolchain
where
    SearchFn: for<'a> FnOnce(&'a Path) -> Option<String>,
{
    if let Some(search) = search_fn
        && let Some(toolchain_str) = search(path)
    {
        return Toolchain::from_str(&toolchain_str).unwrap();
    }

    let exe = get_lowercase_exe(path).unwrap_or_default();
    Toolchain::from_str(&exe).unwrap()
}

impl Toolchain {
    /// Return the canonical command to invoke a detected toolchain.
    ///
    /// The detection first checks the `CC` environment variable, then falls
    /// back to searching the `PATH` for known compiler names.
    pub fn executable_path() -> Option<(Toolchain, PathBuf)> {
        // Check the CC environment variable
        if let Ok(env_cc) = env::var("CC")
            && let Ok(path) = which(&env_cc)
        {
            return Some((detect_toolchain::<NoSearch>(&path, None), path));
        }

        // Search PATH for known compilers
        for name in &["gcc", "clang", "cl", "cc"] {
            if let Ok(path) = which(name) {
                return Some((detect_toolchain::<NoSearch>(&path, None), path));
            }
        }

        None
    }

    pub const fn is_msvc(&self) -> bool {
        matches!(self, Toolchain::Msvc)
    }

    pub const fn is_unix_like(&self) -> bool {
        matches!(self, Toolchain::Gcc | Toolchain::Clang)
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
                Toolchain::Gcc | Toolchain::Clang => {
                    self.link_opts.push(format!("-l{}", lib.as_ref()));
                }
                Toolchain::Msvc => {
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
                Toolchain::Gcc | Toolchain::Clang => {
                    self.link_opts.push(format!("-L{}", path.display()));
                }
                Toolchain::Msvc => {
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

/// Detect the toolchain for the current system.
///
/// Alias for [`Toolchain::executable_path`].
pub fn get_system_compiler() -> Option<(Toolchain, PathBuf)> {
    Toolchain::executable_path()
}
