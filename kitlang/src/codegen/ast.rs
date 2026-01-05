use crate::codegen::types::*;

use std::collections::HashSet;

/// Represents a C header inclusion.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Include {
    /// Path to the header file (e.g., "stdio.h").
    pub path: String,
}

/// Represents a function definition in Kit.
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    /// Function name.
    pub name: String,
    /// List of function parameters.
    pub params: Vec<Param>,
    /// Return type (`None` for void functions).
    pub return_type: Option<Type>,
    /// Function body as a block of statements.
    pub body: Block,
}

/// Represents a function parameter.
#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    /// Parameter name.
    pub name: String,
    /// Parameter type.
    pub ty: Type,
}

/// Represents a block of statements (e.g., function body or scope block).
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    /// List of statements in the block.
    pub stmts: Vec<Stmt>,
}

/// Represents a statement in Kit.
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    /// Variable declaration (with optional type annotation and initializer).
    VarDecl {
        /// Variable name.
        name: String,
        /// Type annotation (`None` for type inference).
        ty: Option<Type>,
        /// Initializer expression (`None` for uninitialized).
        init: Option<Expr>,
    },
    /// Expression statement.
    Expr(Expr),
    /// Return statement (with optional return value).
    Return(Option<Expr>),
    /// If-else statement.
    If {
        /// The condition to evaluate.
        cond: Expr,
        /// The block to execute if the condition is true.
        then_branch: Block,
        /// The block to execute if the condition is false.
        else_branch: Option<Block>,
    },
    /// While loop statement.
    While {
        /// The condition to evaluate.
        cond: Expr,
        /// The block to execute as long as the condition is true.
        body: Block,
    },
    /// For loop statement.
    For {
        /// The name of the loop variable.
        var: String,
        /// The expression to iterate over.
        iter: Expr,
        /// The block to execute for each iteration.
        body: Block,
    },
    /// Break statement.
    Break,
    /// Continue statement.
    Continue,
}

/// Represents an expression in Kit.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// Variable or function identifier.
    Identifier(String),
    /// Literal value.
    Literal(Literal),
    /// Function call.
    Call {
        /// Name of the callee function.
        callee: String,
        /// Arguments passed to the function.
        args: Vec<Expr>,
    },
    /// Unary operation.
    UnaryOp {
        /// The unary operator.
        op: UnaryOperator,
        /// The operand expression.
        expr: Box<Expr>,
    },
    /// Binary operation.
    BinaryOp {
        op: BinaryOperator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Assignment operation.
    Assign {
        op: AssignmentOperator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// If-then-else expression.
    If {
        /// The condition to evaluate.
        cond: Box<Expr>,
        /// The expression to evaluate if the condition is true.
        then_branch: Box<Expr>,
        /// The expression to evaluate if the condition is false.
        else_branch: Box<Expr>,
    },
    /// Range literal expression (e.g., `1...10`).
    RangeLiteral {
        /// The start of the range.
        start: Box<Expr>,
        /// The end of the range (inclusive).
        end: Box<Expr>,
    },
}

/// Represents literal values in Kit.
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    /// Signed integer literal.
    Int(i64),
    /// Floating-point literal.
    Float(f64),
    /// String literal (without quotes).
    String(String),
    /// Boolean literal.
    Bool(bool),
    /// Null pointer literal.
    Null,
}

impl Literal {
    /// Converts the literal to its C representation string.
    pub fn to_c(&self) -> String {
        match self {
            Literal::Int(i) => i.to_string(),
            Literal::Float(f) => {
                // Ensure float literals have 'f' suffix in C
                if f.fract() == 0.0 {
                    format!("{}.0f", f)
                } else {
                    format!("{}f", f)
                }
            }
            Literal::String(s) => {
                // Escape special characters for C string literals
                let escaped: String = s
                    .chars()
                    .map(|c| match c {
                        '\\' => "\\\\".to_string(),
                        '\"' => "\\\"".to_string(),
                        '\n' => "\\n".to_string(),
                        '\t' => "\\t".to_string(),
                        _ => c.to_string(),
                    })
                    .collect();
                format!("\"{}\"", escaped)
            }
            Literal::Bool(b) => b.to_string(),
            Literal::Null => "NULL".to_string(),
        }
    }
}

/// A fully parsed Kit program.
#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    /// C header inclusions required by the program.
    pub includes: Vec<Include>,
    /// Kit module imports (not directly used in C generation).
    pub imports: HashSet<String>,
    /// Top-level function definitions.
    pub functions: Vec<Function>,
}
