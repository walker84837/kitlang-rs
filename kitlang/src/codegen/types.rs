use std::collections::HashSet;
use std::str::FromStr;

/// Trait for converting types to their C representation.
///
/// This trait should be implemented for any type that needs to be represented in generated C code.
/// The conversion should include necessary header dependencies and any required type declarations.
pub trait ToCRepr {
    /// Converts `self` to its C representation.
    fn to_c_repr(&self) -> CType;
}

// Blanket implementation for references to types that implement ToCRepr.
// This allows calling `to_c_repr()` on references without explicit dereferencing.
impl<T: ToCRepr> ToCRepr for &T {
    fn to_c_repr(&self) -> CType {
        (*self).to_c_repr()
    }
}

/// Represents a type in the Kit language.
///
/// This enum covers both primitive C types and composite types. Note that floating-point variants
/// don't implement `Eq` or `Hash` by default, but we manually derive `PartialEq` and `Hash` for
/// practical usage in the compiler. The `Hash` implementation treats floating-point types as
/// having fixed bit patterns (which is valid since we only hash known constant types).
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Type {
    /// User-defined named type (fallback for types not covered by other variants).
    Named(String),
    /// Pointer type (e.g., `Ptr(Int)` represents `int*`).
    Ptr(Box<Type>),
    /// 8-bit signed integer (`int8_t` in C).
    Int8,
    /// 16-bit signed integer (`int16_t` in C).
    Int16,
    /// 32-bit signed integer (`int32_t` in C).
    Int32,
    /// 64-bit signed integer (`int64_t` in C).
    Int64,
    /// 8-bit unsigned integer (`uint8_t` in C).
    Uint8,
    /// 16-bit unsigned integer (`uint16_t` in C).
    Uint16,
    /// 32-bit unsigned integer (`uint32_t` in C).
    Uint32,
    /// 64-bit unsigned integer (`uint64_t` in C).
    Uint64,
    /// 32-bit floating point (`float` in C).
    Float32,
    /// 64-bit floating point (`double` in C).
    Float64,
    /// Platform-dependent integer size (`int` in C).
    Int,
    /// Single-precision floating point (`float` in C).
    Float,
    /// Platform-dependent size type (`size_t` in C).
    Size,
    /// Character type (`char` in C).
    Char,
    /// Boolean type (`bool` from <stdbool.h> in C).
    Bool,
    /// C-style null-terminated string (`char*` in C).
    CString,
    /// Tuple type (represented as a struct in C).
    Tuple(Vec<Type>),
    /// C array type (fixed or variable length).
    ///
    /// The second field is `Some(n)` for fixed-size arrays or `None` for variable-length arrays.
    CArray(Box<Type>, Option<usize>),
}

impl Type {
    /// Converts a Kit type name to its internal representation.
    ///
    /// This handles built-in types directly and falls back to `Named` for user-defined types.
    pub fn from_kit(s: &str) -> Self {
        match s {
            "Int" => Type::Int,
            "Float" => Type::Float,
            "Char" => Type::Char,
            "Size" => Type::Size,
            "CString" => Type::CString,
            other => Type::Named(other.to_string()),
        }
    }
}

/// Represents a C header inclusion.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Include {
    /// Path to the header file (e.g., "stdio.h" or "<stdio.h>").
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
}

/// Unary operators supported in Kit expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    /// Logical NOT (`!`).
    Not,
    /// Arithmetic negation (`-`).
    Negate,
    /// Address-of operator (`&`).
    AddressOf,
    /// Pointer dereference (`*`).
    Dereference,
    /// Prefix increment (`++`).
    Increment,
    /// Prefix decrement (`--`).
    Decrement,
    /// Bitwise NOT (`~`).
    BitwiseNot,
}

impl UnaryOperator {
    /// Formats the operator with its operand as a C expression string.
    pub fn to_string_with_expr(&self, expr: impl Into<String>) -> String {
        let expr = expr.into();
        match self {
            UnaryOperator::Not => format!("!{}", expr),
            UnaryOperator::Negate => format!("-{}", expr),
            UnaryOperator::AddressOf => format!("&{}", expr),
            UnaryOperator::Dereference => format!("*{}", expr),
            UnaryOperator::Increment => format!("++{}", expr),
            UnaryOperator::Decrement => format!("--{}", expr),
            UnaryOperator::BitwiseNot => format!("~{}", expr),
        }
    }
}

impl FromStr for UnaryOperator {
    type Err = ();

    /// Parses a unary operator from its string representation.
    ///
    /// Returns `Err(())` for invalid operator strings.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "!" => Ok(UnaryOperator::Not),
            "-" => Ok(UnaryOperator::Negate),
            "&" => Ok(UnaryOperator::AddressOf),
            "*" => Ok(UnaryOperator::Dereference),
            "++" => Ok(UnaryOperator::Increment),
            "--" => Ok(UnaryOperator::Decrement),
            "~" => Ok(UnaryOperator::BitwiseNot),
            _ => Err(()),
        }
    }
}

/// Binary operators supported in Kit expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    // Additive
    Add,
    Subtract,
    // Multiplicative
    Multiply,
    Divide,
    Modulo,
    // Equality
    Eq,
    Neq,
    // Comparison
    Gt,
    Gte,
    Lt,
    Lte,
    // Logical
    And,
    Or,
    // Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
}

impl FromStr for BinaryOperator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(BinaryOperator::Add),
            "-" => Ok(BinaryOperator::Subtract),
            "*" => Ok(BinaryOperator::Multiply),
            "/" => Ok(BinaryOperator::Divide),
            "%" => Ok(BinaryOperator::Modulo),
            "==" => Ok(BinaryOperator::Eq),
            "!=" => Ok(BinaryOperator::Neq),
            ">" => Ok(BinaryOperator::Gt),
            ">=" => Ok(BinaryOperator::Gte),
            "<" => Ok(BinaryOperator::Lt),
            "<=" => Ok(BinaryOperator::Lte),
            "&&" => Ok(BinaryOperator::And),
            "||" => Ok(BinaryOperator::Or),
            "&" => Ok(BinaryOperator::BitwiseAnd),
            "|" => Ok(BinaryOperator::BitwiseOr),
            "^" => Ok(BinaryOperator::BitwiseXor),
            "<<" => Ok(BinaryOperator::BitwiseLeftShift),
            ">>" => Ok(BinaryOperator::BitwiseRightShift),
            _ => Err(()),
        }
    }
}

impl BinaryOperator {
    pub fn to_c_str(&self) -> &'static str {
        match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Modulo => "%",
            BinaryOperator::Eq => "==",
            BinaryOperator::Neq => "!=",
            BinaryOperator::Gt => ">",
            BinaryOperator::Gte => ">=",
            BinaryOperator::Lt => "<",
            BinaryOperator::Lte => "<=",
            BinaryOperator::And => "&&",
            BinaryOperator::Or => "||",
            BinaryOperator::BitwiseAnd => "&",
            BinaryOperator::BitwiseOr => "|",
            BinaryOperator::BitwiseXor => "^",
            BinaryOperator::BitwiseLeftShift => "<<",
            BinaryOperator::BitwiseRightShift => ">>",
        }
    }
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
    /// If-then-else expression.
    If {
        /// The condition to evaluate.
        cond: Box<Expr>,
        /// The expression to evaluate if the condition is true.
        then_branch: Box<Expr>,
        /// The expression to evaluate if the condition is false.
        else_branch: Box<Expr>,
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

/// C type representation for code generation.
///
/// This struct encapsulates all information needed to generate a C type:
/// - The type name as it appears in C code
/// - Required header dependencies
/// - Optional type declaration (for structs or typedefs)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CType {
    /// The C type name (e.g., "int", "MyStruct", "uint32_t").
    pub name: String,
    /// Headers required for this type (e.g., ["<stdint.h>"]).
    pub headers: Vec<String>,
    /// Custom declaration needed for this type (e.g., struct definitions).
    /// `None` for primitive/built-in types.
    pub declaration: Option<String>,
}

impl CType {
    /// Creates a new C type representation with no headers or declaration.
    fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            headers: Vec::new(),
            declaration: None,
        }
    }

    /// Creates a C type that requires a specific header.
    fn with_header(name: impl Into<String>, header: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            headers: vec![header.into()],
            declaration: None,
        }
    }
}

/// Creates a sanitized C identifier from a Kit type.
///
/// This is used for generating struct names for tuples and arrays. The identifier:
/// - Uses standard C fixed-width integer names (e.g., "int32_t" instead of "i32")
/// - Uses "float" and "double" for floating-point types
/// - Prefixes composite types with "__Kit" to avoid naming conflicts
/// - Escapes type structures into valid C identifiers
fn type_to_c_ident_string(t: &Type) -> String {
    match t {
        Type::Named(s) => s.clone(),
        Type::Ptr(inner) => format!("{}_ptr", type_to_c_ident_string(inner)),
        Type::Int8 => "int8_t".to_string(),
        Type::Int16 => "int16_t".to_string(),
        Type::Int32 => "int32_t".to_string(),
        Type::Int64 => "int64_t".to_string(),
        Type::Uint8 => "uint8_t".to_string(),
        Type::Uint16 => "uint16_t".to_string(),
        Type::Uint32 => "uint32_t".to_string(),
        Type::Uint64 => "uint64_t".to_string(),
        Type::Float32 => "float".to_string(),
        Type::Float64 => "double".to_string(),
        Type::Int => "int".to_string(),
        Type::Float => "float".to_string(),
        Type::Size => "size_t".to_string(),
        Type::Char => "char".to_string(),
        Type::Bool => "bool".to_string(),
        Type::CString => "cstring".to_string(),
        Type::Tuple(types) => {
            let member_types = types
                .iter()
                .map(type_to_c_ident_string)
                .collect::<Vec<_>>()
                .join("_");
            format!("__KitTuple_{}", member_types)
        }
        Type::CArray(inner, _) => format!("{}__KitArray", type_to_c_ident_string(inner)),
    }
}

impl ToCRepr for Type {
    fn to_c_repr(&self) -> CType {
        match self {
            Type::Int8 => CType::with_header("int8_t", "<stdint.h>"),
            Type::Int16 => CType::with_header("int16_t", "<stdint.h>"),
            Type::Int32 => CType::with_header("int32_t", "<stdint.h>"),
            Type::Int64 => CType::with_header("int64_t", "<stdint.h>"),
            Type::Uint8 => CType::with_header("uint8_t", "<stdint.h>"),
            Type::Uint16 => CType::with_header("uint16_t", "<stdint.h>"),
            Type::Uint32 => CType::with_header("uint32_t", "<stdint.h>"),
            Type::Uint64 => CType::with_header("uint64_t", "<stdint.h>"),
            Type::Float32 => CType::new("float"),
            Type::Float64 => CType::new("double"),
            Type::Int => CType::new("int"),
            Type::Float => CType::new("float"),
            Type::Size => CType::with_header("size_t", "<stddef.h>"),
            Type::Char => CType::new("char"),
            Type::Bool => CType::with_header("bool", "<stdbool.h>"),
            Type::CString => CType::new("char*"),
            Type::Ptr(inner) => {
                let mut c = inner.to_c_repr();
                // Add pointer asterisk to the type name
                c.name.push('*');
                c
            }
            Type::Tuple(fields) => {
                let type_names_mangled = fields
                    .iter()
                    .map(type_to_c_ident_string)
                    .collect::<Vec<_>>()
                    .join("_");

                let struct_name = format!("__KitTuple_{}", type_names_mangled);

                let mut all_headers = HashSet::new();
                let mut all_declarations = Vec::new();

                let members = fields
                    .iter()
                    .enumerate()
                    .map(|(i, f)| {
                        let c = f.to_c_repr();
                        for header in &c.headers {
                            all_headers.insert(header.clone());
                        }
                        if let Some(decl) = &c.declaration {
                            all_declarations.push(decl.clone());
                        }
                        format!("    {} _{};\n", c.name, i)
                    })
                    .collect::<String>();

                all_declarations.push(format!(
                    "typedef struct {{\n{}}} {};\n",
                    members, struct_name
                ));

                CType {
                    name: struct_name,
                    headers: all_headers.into_iter().collect(),
                    declaration: Some(all_declarations.join("\n")),
                }
            }
            Type::CArray(elem, len) => {
                let base = elem.to_c_repr();
                if let Some(n) = len {
                    // Fixed-size array: int[10]
                    let mut ctype = base;
                    ctype.name = format!("{}[{}]", ctype.name, n);
                    ctype
                } else {
                    // Variable-length array: represented as struct { size_t len; T* data; }
                    let type_name_mangled = type_to_c_ident_string(elem);
                    let struct_name = format!("__KitArray_{}", type_name_mangled);
                    let decl = format!(
                        "typedef struct {{\n    size_t len;\n    {} *data;\n}} {};\n",
                        base.name, struct_name
                    );

                    let mut all_headers: HashSet<String> = base.headers.into_iter().collect();
                    all_headers.insert("<stddef.h>".to_string());

                    let mut all_declarations = Vec::new();
                    if let Some(d) = base.declaration {
                        all_declarations.push(d);
                    }
                    all_declarations.push(decl);

                    CType {
                        name: struct_name,
                        headers: all_headers.into_iter().collect(),
                        declaration: Some(all_declarations.join("\n")),
                    }
                }
            }
            // User-defined types are assumed to be already declared elsewhere
            Type::Named(name) => CType::new(name.to_string()),
        }
    }
}
