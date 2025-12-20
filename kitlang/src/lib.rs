//! This crate contains the core logic for the Kit language compiler,
//! including its lexer, parser, and code generation components.

pub mod codegen;

pub mod lexer;

pub use logos;

/// The Kit language grammar, generated from a pest grammar file.
#[derive(pest_derive::Parser)]
#[grammar = "grammar/kit.pest"]
pub struct KitParser;

mod error;
