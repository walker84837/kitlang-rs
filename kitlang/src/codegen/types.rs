use std::collections::HashSet;
use std::str::FromStr;

pub trait ToCRepr<T> {
    fn to_c_repr(&self) -> T;
}

impl<T: ToCRepr<T>> ToCRepr<T> for &T {
    fn to_c_repr(&self) -> T {
        (*self).to_c_repr()
    }
}

// `Type` has float variants, which don't implement `Eq` or `Hash`.
// We can implement `Hash` manually if we need to store `Type`s in a `HashSet`.
// For now, `PartialEq` is sufficient.
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Type {
    Named(String),  // fallback for user-defined types
    Ptr(Box<Type>), // pointer type, e.g. Ptr[Int]

    // Numeric types
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,

    Int,   // C's `int`
    Float, // C's `float`
    Size,  // C's `size_t`

    Char,    // C `char`
    Bool,    // C `_Bool` or `bool` from <stdbool.h>
    CString, // char* (null-terminated)

    Tuple(Vec<Type>),
    CArray(Box<Type>, Option<usize>),
}

impl Type {
    /// Converts a Kit type name to an internal representation.
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Include {
    pub path: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    VarDecl {
        name: String,
        ty: Option<Type>,
        init: Option<Expr>,
    },
    Expr(Expr),
    Return(Option<Expr>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Not,         // !
    Negate,      // -
    AddressOf,   // &
    Dereference, // *
    Increment,   // ++ (prefix)
    Decrement,   // -- (prefix)
    BitwiseNot,  // ~
}

impl UnaryOperator {
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

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Identifier(String),
    Literal(Literal),
    Call { callee: String, args: Vec<Expr> },
    UnaryOp { op: UnaryOperator, expr: Box<Expr> },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Null,
}

impl Literal {
    pub fn to_c(&self) -> String {
        match self {
            Literal::Int(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Bool(b) => b.to_string(),
            Literal::Null => "NULL".to_string(),
        }
    }
}

/// A parsed Kit program
#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub includes: Vec<Include>,
    pub imports: HashSet<String>,
    pub functions: Vec<Function>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CType {
    pub name: String,
    pub headers: Vec<String>,
    pub declaration: Option<String>,
}

impl CType {
    fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            headers: Vec::new(),
            declaration: None,
        }
    }

    fn with_header(name: impl Into<String>, header: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            headers: vec![header.into()],
            declaration: None,
        }
    }
}

/// Creates a sanitized C identifier from a Kit type.
fn type_to_c_ident_string(t: &Type) -> String {
    match t {
        Type::Named(s) => s.clone(),
        Type::Ptr(inner) => format!("{}_ptr", type_to_c_ident_string(inner)),
        // TODO: these types look more like Rust types than C types? The doc says "sanitized C
        // identifier", but i8 to my knowledge seems more like a Rust type, instead of something
        // like int8_t. I'm not sure if these many types exist in <stdlib.h>
        Type::Int8 => "i8".to_string(),
        Type::Int16 => "i16".to_string(),
        Type::Int32 => "i32".to_string(),
        Type::Int64 => "i64".to_string(),
        Type::Uint8 => "u8".to_string(),
        Type::Uint16 => "u16".to_string(),
        Type::Uint32 => "u32".to_string(),
        Type::Uint64 => "u64".to_string(),
        Type::Float32 => "f32".to_string(),
        Type::Float64 => "f64".to_string(),
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
        Type::CArray(inner, _) => format!("__KitArray_{}", type_to_c_ident_string(inner)),
    }
}

impl ToCRepr<CType> for Type {
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
                        for header in c.headers {
                            all_headers.insert(header);
                        }
                        if let Some(decl) = c.declaration {
                            all_declarations.push(decl);
                        }
                        format!("    {} _{};\n", c.name, i)
                    })
                    .collect::<String>();

                all_declarations.push(format!(
                    "typedef struct {{\n{members}}} {name};",
                    members = members,
                    name = struct_name
                ));

                let final_declaration = all_declarations.join("\n");

                CType {
                    name: struct_name,
                    headers: all_headers.into_iter().collect(),
                    declaration: Some(final_declaration),
                }
            }

            Type::CArray(elem, len) => {
                let base = elem.to_c_repr();
                if let Some(n) = len {
                    // fixed‐size
                    let mut ctype = base;
                    ctype.name = format!("{}[{}]", ctype.name, n);
                    ctype
                } else {
                    // unsized: we generate an in‐place struct hack
                    let type_name_mangled = type_to_c_ident_string(elem);
                    let struct_name = format!("__KitArray_{}", type_name_mangled);
                    let decl = format!(
                        "typedef struct {{ size_t len; {} *data; }} {};",
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

            // User-defined
            Type::Named(name) => CType::new(name.to_string()),
        }
    }
}
