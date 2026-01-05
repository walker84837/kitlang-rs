//! The `codegen` module is responsible for generating executable code
//! from the parsed Kit Abstract Syntax Tree (AST).
//!
//! It orchestrates the compilation process, translating the AST into an
//! intermediate representation and then into target-specific machine code.

pub mod ast;
pub mod compiler;
pub mod parser;
pub use ast::{Block, Expr, Function, Include, Literal, Param, Program, Stmt};
pub use compiler::Toolchain;

/// Handles the initial parsing of Kitlang source files, constructs the
/// Abstract Syntax Tree (AST), and orchestrates the generation of C code
/// from this AST.
pub mod frontend;

/// Defines the core data structures used throughout the code generation process,
/// including representations for Kitlang types, C types, functions, statements,
/// expressions, and literals. It also handles the conversion logic between Kitlang
/// and C type systems.
pub mod types;
