#[derive(Debug)]
pub enum Type {
    Named(String),
    Ptr(Box<Type>),
    Int,
    Float,
    CString,
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
pub enum Expr {
    Identifier(String),
    Literal(Literal),
    Call { callee: String, args: Vec<Expr> },
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
    pub imports: Vec<String>,
    pub functions: Vec<Function>,
}
