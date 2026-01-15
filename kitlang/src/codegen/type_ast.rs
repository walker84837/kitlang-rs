use crate::codegen::types::TypeId;

use super::ast::Expr;
use super::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct StructDefinition {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: TypeId,
    pub annotation: Option<Type>,
    pub is_const: bool,
    pub default: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldInit {
    pub name: String,
    pub value: Expr,
}
