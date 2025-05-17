pub mod codegen;
pub mod lexer;
pub mod typechecking;

pub use logos;

#[derive(pest_derive::Parser)]
#[grammar = "grammar/kit.pest"]
pub struct KitParser;
