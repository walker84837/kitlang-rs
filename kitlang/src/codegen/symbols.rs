use super::types::TypeId;
use std::collections::HashMap;

/// Symbol table for tracking variable and function types during inference.
///
/// Currently uses a flat scope (no nesting). Variables and functions are tracked
/// by their names and their TypeIds.
pub struct SymbolTable {
    /// Maps variable names to their inferred TypeIds.
    vars: HashMap<String, TypeId>,

    /// Maps function names to their signatures (parameter types, return type).
    functions: HashMap<String, (Vec<TypeId>, TypeId)>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    /// Define a variable in the current scope.
    pub fn define_var(&mut self, name: &str, ty: TypeId) {
        self.vars.insert(name.to_string(), ty);
    }

    /// Look up a variable's type.
    pub fn lookup_var(&self, name: &str) -> Option<TypeId> {
        self.vars.get(name).copied()
    }

    /// Define a function signature.
    pub fn define_function(&mut self, name: &str, params: Vec<TypeId>, ret: TypeId) {
        self.functions.insert(name.to_string(), (params, ret));
    }

    /// Look up a function's signature.
    pub fn lookup_function(&self, name: &str) -> Option<(Vec<TypeId>, TypeId)> {
        self.functions.get(name).cloned()
    }
}
