use std::collections::HashSet;

#[derive(Debug)]
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

    Int, // C's `int`
    Float,
    Size, // C's `size_t`

    Char,    // C `char`
    Bool,    // C `_Bool` or `bool` from <stdbool.h>
    CString, // char* (null-terminated)

    Tuple(Vec<Type>),
    CArray(Box<Type>, Option<usize>),
}

#[derive(Clone, Debug)]
pub struct Include {
    pub path: String,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    VarDecl {
        name: String,
        ty: Option<Type>,
        init: Option<Expr>,
    },
    Expr(Expr),
    Return(Option<Expr>),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Not,         // !
    Negate,      // -
    AddressOf,   // &
    Dereference, // *
    Increment,   // ++ (prefix)
    Decrement,   // -- (prefix)
    BitwiseNot,  // ~
}

#[derive(Debug)]
pub enum Expr {
    Identifier(String),
    Literal(Literal),
    Call { callee: String, args: Vec<Expr> },
    UnaryOp { op: UnaryOperator, expr: Box<Expr> },
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Null,
}

/// A parsed Kit program
#[derive(Debug)]
pub struct Program {
    pub includes: Vec<Include>,
    pub imports: HashSet<String>,
    pub functions: Vec<Function>,
}

pub struct CType {
    pub name: String,
    pub required_header: Option<String>,
}

impl From<(&'static str, Option<&'static str>)> for CType {
    fn from((nm, hdr): (&'static str, Option<&'static str>)) -> Self {
        CType {
            name: nm.into(),
            required_header: hdr.map(wrap_header),
        }
    }
}

impl From<(String, Option<&'static str>)> for CType {
    fn from((nm, hdr): (String, Option<&'static str>)) -> Self {
        CType {
            name: nm,
            required_header: hdr.map(wrap_header),
        }
    }
}

/// Wraps a string in angle brackets (header.h) if it doesn't already have them
fn wrap_header(h: &'static str) -> String {
    if h.starts_with('<') && h.ends_with('>') {
        h.to_string()
    } else {
        format!("<{}>", h)
    }
}

macro_rules! specific_int_type {
    ($name:literal) => {
        CType {
            name: $name.into(),
            required_header: Some("<stdint.h>".into()),
        }
    };
}

pub(crate) fn type_to_c(t: &Type) -> CType {
    match t {
        Type::Int8 => specific_int_type!("int8_t"),
        Type::Int16 => specific_int_type!("int16_t"),
        Type::Int32 => specific_int_type!("int32_t"),
        Type::Int64 => specific_int_type!("int64_t"),
        Type::Uint8 => specific_int_type!("uint8_t"),
        Type::Uint16 => specific_int_type!("uint16_t"),
        Type::Uint32 => specific_int_type!("uint32_t"),
        Type::Uint64 => specific_int_type!("uint64_t"),

        Type::Float32 => ("float", None).into(),
        Type::Float64 => ("double", None).into(),

        Type::Int => ("int", None).into(),
        Type::Float => ("float", None).into(),
        Type::Size => ("size_t", Some("<stddef.h>")).into(),
        Type::Char => ("char", None).into(),
        Type::Bool => ("bool", Some("<stdbool.h>")).into(),

        Type::CString => ("char*", None).into(),

        Type::Ptr(inner) => {
            let mut c = type_to_c(inner);
            c.name.push('*');
            c
        }

        Type::Tuple(fields) => {
            // generate a unique name, e.g. __KitTuple3
            let struct_name = format!("__KitTuple{}", fields.len());

            // build members: "    <ctype> _0;\n    <ctype> _1; ..."
            let members = fields
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    let c = type_to_c(f);
                    format!("    {} _{};\n", c.name, i)
                })
                .collect::<String>();

            let decl = format!(
                "typedef struct {{\n{members}}} {name};\n",
                members = members,
                name = struct_name
            );

            CType {
                name: struct_name,
                required_header: Some(decl),
            }
        }

        Type::CArray(elem, len) => {
            let mut base = type_to_c(elem);
            if let Some(n) = len {
                // fixed‐size
                base.name = format!("{}[{}]", base.name, n);
                base
            } else {
                // unsized: we generate an in‐place struct hack
                let decl = format!(
                    "typedef struct {{ size_t len; {} *data; }} __KitArray{};\n",
                    base.name, /* unique id? */ 0
                );
                CType {
                    name: format!("__KitArray{}", 0),
                    required_header: Some(decl),
                }
            }
        }

        // User-defined
        Type::Named(name) => (name.to_string(), None).into(),
    }
}
