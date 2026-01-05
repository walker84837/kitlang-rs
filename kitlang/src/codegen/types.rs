use crate::error::CompilationError;
use crate::Rule;

use pest::iterators::Pair;

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
    /// Represents a void type (e.g., for functions with no return value).
    Void,
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
            "Bool" => Type::Bool,
            "Void" => Type::Void,
            other => Type::Named(other.to_string()),
        }
    }
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
    /// e.g. `to_string_with_expr("++", "x") -> "++x"`
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
    /// Additive
    Add,
    /// Subtractive: `-`
    Subtract,
    // Multiplicative
    Multiply,
    /// Divide
    Divide,
    /// Modulo
    Modulo,
    /// Equality
    Eq,
    /// Negative equality
    Neq,
    /// Greater than
    Gt,
    /// Greater than or equal
    Gte,
    /// Less than
    Lt,
    /// Less than or equal
    Lte,
    // Logical
    And,
    /// Logical
    Or,
    /// Bitwise AND
    BitwiseAnd,
    /// Bitwise OR
    BitwiseOr,
    /// Bitwise XOR
    BitwiseXor,
    /// Bitwise left shift
    BitwiseLeftShift,
    /// Bitwise right shift
    BitwiseRightShift,
}

impl BinaryOperator {
    pub fn from_rule_pair(pair: &Pair<Rule>) -> Result<Self, CompilationError> {
        match pair.as_rule() {
            Rule::additive_op => match pair.as_str() {
                "+" => Ok(BinaryOperator::Add),
                "-" => Ok(BinaryOperator::Subtract),
                _ => Err(CompilationError::InvalidOperator(format!(
                    "Unknown additive operator: {}",
                    pair.as_str()
                ))),
            },
            Rule::multiplicative_op => match pair.as_str() {
                "*" => Ok(BinaryOperator::Multiply),
                "/" => Ok(BinaryOperator::Divide),
                "%" => Ok(BinaryOperator::Modulo),
                _ => Err(CompilationError::InvalidOperator(format!(
                    "Unknown multiplicative operator: {}",
                    pair.as_str()
                ))),
            },
            Rule::eq_op => match pair.as_str() {
                "==" => Ok(BinaryOperator::Eq),
                "!=" => Ok(BinaryOperator::Neq),
                _ => Err(CompilationError::InvalidOperator(format!(
                    "Unknown equality operator: {}",
                    pair.as_str()
                ))),
            },
            Rule::comp_op => match pair.as_str() {
                ">" => Ok(BinaryOperator::Gt),
                ">=" => Ok(BinaryOperator::Gte),
                "<" => Ok(BinaryOperator::Lt),
                "<=" => Ok(BinaryOperator::Lte),
                _ => Err(CompilationError::InvalidOperator(format!(
                    "Unknown comparison operator: {}",
                    pair.as_str()
                ))),
            },
            Rule::and_ops => match pair.as_str() {
                "&&" => Ok(BinaryOperator::And),
                "&" => Ok(BinaryOperator::BitwiseAnd),
                _ => unreachable!(), // Should not happen with atomic rules
            },
            Rule::logical_or_op => Ok(BinaryOperator::Or), // Use new logical_or_op
            Rule::bitwise_or_op => Ok(BinaryOperator::BitwiseOr), // Use new bitwise_or_op
            Rule::bitwise_xor_op => Ok(BinaryOperator::BitwiseXor),
            Rule::shift_op => match pair.as_str() {
                "<<" => Ok(BinaryOperator::BitwiseLeftShift),
                ">>" => Ok(BinaryOperator::BitwiseRightShift),
                _ => Err(CompilationError::InvalidOperator(format!(
                    "Unknown shift operator: {}",
                    pair.as_str()
                ))),
            },
            _ => Err(CompilationError::InvalidOperator(format!(
                "Unexpected rule for binary operator: {:?}",
                pair.as_rule()
            ))),
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

/// Assignment operators supported in Kit expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AssignmentOperator {
    /// Simple assignment (`=`).
    Assign,
    /// Add and assign (`+=`).
    AddAssign,
    /// Subtract and assign (`-=`).
    SubtractAssign,
    /// Multiply and assign (`*=`).
    MultiplyAssign,
    /// Divide and assign (`/=`).
    DivideAssign,
    /// Modulo and assign (`%=`).
    ModuloAssign,
    /// Bitwise AND and assign (`&=`).
    BitwiseAndAssign,
    /// Bitwise OR and assign (`|=`).
    BitwiseOrAssign,
    /// Bitwise XOR and assign (`^=`).
    BitwiseXorAssign,
    /// Bitwise left shift and assign (`<<=`).
    BitwiseLeftShiftAssign,
    /// Bitwise right shift and assign (`>>=`).
    BitwiseRightShiftAssign,
}

impl AssignmentOperator {
    pub fn from_rule_pair(pair: &Pair<Rule>) -> Result<Self, CompilationError> {
        match pair.as_rule() {
            Rule::ASSIGN_OP => match pair.as_str() {
                "=" => Ok(AssignmentOperator::Assign),
                "+=" => Ok(AssignmentOperator::AddAssign),
                "-=" => Ok(AssignmentOperator::SubtractAssign),
                "*=" => Ok(AssignmentOperator::MultiplyAssign),
                "/=" => Ok(AssignmentOperator::DivideAssign),
                "%=" => Ok(AssignmentOperator::ModuloAssign),
                "&=" => Ok(AssignmentOperator::BitwiseAndAssign),
                "|=" => Ok(AssignmentOperator::BitwiseOrAssign),
                "^=" => Ok(AssignmentOperator::BitwiseXorAssign),
                "<<=" => Ok(AssignmentOperator::BitwiseLeftShiftAssign),
                ">>=" => Ok(AssignmentOperator::BitwiseRightShiftAssign),
                _ => Err(CompilationError::InvalidOperator(format!(
                    "Unknown assignment operator: {}",
                    pair.as_str()
                ))),
            },
            _ => Err(CompilationError::InvalidOperator(format!(
                "Unexpected rule for assignment operator: {:?}",
                pair.as_rule()
            ))),
        }
    }
}

impl AssignmentOperator {
    pub fn to_c_str(&self) -> &'static str {
        match self {
            AssignmentOperator::Assign => "=",
            AssignmentOperator::AddAssign => "+=",
            AssignmentOperator::SubtractAssign => "-=",
            AssignmentOperator::MultiplyAssign => "*=",
            AssignmentOperator::DivideAssign => "/=",
            AssignmentOperator::ModuloAssign => "%=",
            AssignmentOperator::BitwiseAndAssign => "&=",
            AssignmentOperator::BitwiseOrAssign => "|=",
            AssignmentOperator::BitwiseXorAssign => "^=",
            AssignmentOperator::BitwiseLeftShiftAssign => "<<=",
            AssignmentOperator::BitwiseRightShiftAssign => ">>=",
        }
    }
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
        Type::Void => "void".to_string(),
    }
}

impl ToCRepr for Type {
    fn to_c_repr(&self) -> CType {
        // small helper to reduce repetition for header-backed types
        fn hdr(name: &str, header: &str) -> CType {
            CType::with_header(name, header)
        }

        match self {
            // fixed-width integer types -> <stdint.h>
            Type::Int8 => hdr("int8_t", "<stdint.h>"),
            Type::Int16 => hdr("int16_t", "<stdint.h>"),
            Type::Int32 => hdr("int32_t", "<stdint.h>"),
            Type::Int64 => hdr("int64_t", "<stdint.h>"),
            Type::Uint8 => hdr("uint8_t", "<stdint.h>"),
            Type::Uint16 => hdr("uint16_t", "<stdint.h>"),
            Type::Uint32 => hdr("uint32_t", "<stdint.h>"),
            Type::Uint64 => hdr("uint64_t", "<stdint.h>"),

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
                c.name = format!("{}*", c.name); // clearer than push
                c
            }

            // Transform a tuple type into a C struct definition
            Type::Tuple(fields) => {
                // Mangle each field's C identifier and join with '_', e.g., "i32_f64_..."
                // to avoid name conflicts when tuples with same size have different types.
                // In practice, this makes (Int, Int) and (Float, Float) two entirely
                // different types when converted to C.
                let type_names_mangled = fields
                    .iter()
                    .map(type_to_c_ident_string)
                    .collect::<Vec<_>>()
                    .join("_");

                // Build a unique struct name using the mangled field list
                let struct_name = format!("__KitTuple_{}", type_names_mangled);

                // Collect all required headers and declarations from the fields
                let mut all_headers = HashSet::new();
                let mut all_declarations = Vec::new();

                // Generate the struct members (like "int _0;") and gather headers/decls
                let members = fields
                    .iter()
                    .enumerate()
                    .map(|(i, f)| {
                        let c = f.to_c_repr();

                        // collect needed headers and declarations
                        all_headers.extend(c.headers);
                        if let Some(decl) = c.declaration {
                            all_declarations.push(decl);
                        }

                        format!("    {} _{};\n", c.name, i) // struct member line
                    })
                    .collect::<String>();

                // Append the full typedef for the tuple struct
                all_declarations.push(format!(
                    "typedef struct {{\n{}}} {};\n",
                    members, struct_name
                ));

                // Return the C type description for the tuple
                CType {
                    name: struct_name,
                    headers: all_headers.into_iter().collect(),
                    declaration: Some(all_declarations.join("\n")),
                }
            }

            // Transform a C-array type
            Type::CArray(elem, len) => {
                let base = elem.to_c_repr();

                // Fixed-size array, like int[10]
                if let Some(n) = len {
                    let mut ctype = base;
                    ctype.name = format!("{}[{}]", ctype.name, n);
                    ctype
                // Variable-length array: convert to wrapper struct with length + pointer
                } else {
                    let type_name_mangled = type_to_c_ident_string(elem);
                    let struct_name = format!("__KitArray_{}", type_name_mangled);
                    let decl = format!(
                        "typedef struct {{\n    size_t len;\n    {} *data;\n}} {};\n",
                        base.name, struct_name
                    );

                    // Gather headers (need <stddef.h> for size_t) and any nested decls
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
            Type::Void => CType::new("void"),

            // User-defined types are assumed to be already declared elsewhere
            Type::Named(name) => CType::new(name.to_string()),
        }
    }
}
