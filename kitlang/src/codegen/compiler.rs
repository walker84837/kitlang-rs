use std::convert::Infallible;
use std::env;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use which::which;

type NoSearch = fn(&Path) -> Option<String>;

#[derive(Debug, Clone, Copy)]
pub enum Toolchain {
    Gcc,
    Clang,
    #[cfg(windows)]
    Msvc,
    Other,
}

impl FromStr for Toolchain {
    type Err = Infallible;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Ok(match value {
            // TODO: assume it's GCC, but this could also be clang.
            // This *should* be fine as both take almost the same arguments, but this should be
            // changed later on.
            "gcc" | "cc" => Toolchain::Gcc,
            "clang" => Toolchain::Clang,
            #[cfg(windows)]
            "cl" => Toolchain::Msvc,
            _ => Toolchain::Other,
        })
    }
}

// TODO: Fix this doctest from failing because of imports
/// Convert a path's file stem to a lowercase `String`.
///
/// This attempts to get the file stem (the filename without extension)
/// from the provided `Path`, convert it to UTF‑8, and return a lowercase
/// `String`.
///
/// If the path has no file stem or the file stem is not valid UTF‑8, this function returns `None`.
///
/// # Examples
///
/// ```ignore
/// # use std::path::Path;
/// # use kitlang::codegen::compiler::get_lowercase_exe;
///
/// assert_eq!(
///     get_lowercase_exe(Path::new("/foo/Bar.TXT")),
///     Some("bar".to_string())
/// );
///
/// // No file stem (path ends with `/`)
/// assert_eq!(get_lowercase_exe(Path::new("/foo/")), None);
/// ```
///
/// # Returns
///
/// - `Some(String)` containing the lowercase file stem if present and valid UTF‑8.
/// - `None` if there is no file stem or it cannot be represented as UTF‑8.
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
            // Search PATH for known compilers
            let search_fn = |_: &Path| {
                let compilers = ["clang", "gcc"];

                // Look through each possible compiler and check if it exists
                let found = compilers.iter().map(which::which).find_map(Result::ok);

                // Get the executable name from the path
                found.as_deref().and_then(get_lowercase_exe)
            };

            return Some((detect_toolchain(&path, Some(search_fn)), path));
        }

        let candidates = &["cc", "clang", "gcc"];

        for name in candidates {
            if let Ok(path) = which(name) {
                let toolchain = if cfg!(unix) && *name == "cc" {
                    resolve_cc_toolchain(&path)
                } else {
                    detect_toolchain::<NoSearch>(&path, None)
                };
                return Some((toolchain, path));
            }
        }

        None
    }

    pub const fn is_msvc(&self) -> bool {
        #[cfg(windows)]
        {
            matches!(self, Toolchain::Msvc)
        }
        #[cfg(not(windows))]
        {
            // MSVC isn't on other OSes, so it's always false outside of Windows
            false
        }
    }

    pub const fn is_unix_like(&self) -> bool {
        matches!(self, Toolchain::Gcc | Toolchain::Clang)
    }

    pub fn get_compiler_flags(&self) -> Vec<String> {
        match self {
            Toolchain::Gcc | Toolchain::Clang => {
                let flags = ["-std=c99", "-Wall", "-Wextra", "-pedantic"];
                // The C compiler (gcc/clang) by default searches standard system include paths
                // so we don't necessarily need to add -I/usr/include explicitly here
                // unless we want to override or add specific custom paths.
                // convert it to a vector of strings
                flags.iter().map(|s| s.to_string()).collect()
            }
            #[cfg(windows)]
            Toolchain::Msvc => {
                // MSVC equivalent flags for C99 (or closest standard)
                // /std:c11 or /std:c17 for newer, /Za for ANSI C compliance
                vec!["/std:c11".to_string(), "/W4".to_string()] // /W4 for high warning level
            }
            Toolchain::Other => {
                // For unknown toolchains, return minimal safe flags
                vec![]
            }
        }
    }
}

/// `cc` is often a symlink to an actual compiler on the system, so
/// we need to get an actual path to the C compiler.
fn resolve_cc_toolchain(path: &Path) -> Toolchain {
    if cfg!(unix)
        && path.ends_with("cc")
        && let Ok(real_path) = std::fs::read_link(path)
    {
        return detect_toolchain::<NoSearch>(&real_path, None);
    }
    detect_toolchain::<NoSearch>(path, None)
}

#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub toolchain: Toolchain,
    pub target: Vec<PathBuf>,
    pub link_opts: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct CompilerMeta(pub Toolchain);

impl CompilerOptions {
    pub fn new(base_meta: CompilerMeta) -> Self {
        Self {
            toolchain: base_meta.0,
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
                #[cfg(windows)]
                Toolchain::Msvc => {
                    self.link_opts.push(format!("{}.lib", lib.as_ref()));
                }
                Toolchain::Other => {}
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
                #[cfg(windows)]
                Toolchain::Msvc => {
                    self.link_opts.push(format!("/LIBPATH:{}", path.display()));
                }
                Toolchain::Other => {}
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
