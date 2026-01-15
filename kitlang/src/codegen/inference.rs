use super::ast::{Block, Expr, Function, Literal, Program, Stmt};
use super::symbols::SymbolTable;
use super::type_ast::StructDefinition;
use super::types::{BinaryOperator, Type, TypeId, TypeStore, UnaryOperator};
use crate::error::{CompilationError, CompileResult};

/// Type inference engine using Hindley-Milner algorithm.
pub struct TypeInferencer {
    pub store: TypeStore,
    symbols: SymbolTable,
    current_return_type: Option<TypeId>,
}

impl Default for TypeInferencer {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInferencer {
    pub fn new() -> Self {
        Self {
            store: TypeStore::new(),
            symbols: SymbolTable::new(),
            current_return_type: None,
        }
    }

    /// Infer types for an entire program
    pub fn infer_program(&mut self, prog: &mut Program) -> CompileResult<()> {
        // First pass: register struct types
        self.register_struct_types(&prog.structs)?;

        // Second pass: infer function types
        for func in &mut prog.functions {
            self.infer_function(func)?;
        }
        Ok(())
    }

    /// Register struct types in the type store
    fn register_struct_types(&mut self, structs: &[StructDefinition]) -> CompileResult<()> {
        for struct_def in structs {
            // Build field type list
            let field_types: Vec<(String, TypeId)> = struct_def
                .fields
                .iter()
                .map(|field| {
                    let field_type_id = if let Some(ann) = &field.annotation {
                        self.store.new_known(ann.clone())
                    } else {
                        // Fields should have type annotations, but if not, use unknown
                        self.store.new_unknown()
                    };
                    (field.name.clone(), field_type_id)
                })
                .collect();

            // Create struct type and register it
            let struct_type = Type::Struct {
                name: struct_def.name.clone(),
                fields: field_types.clone(),
            };

            let _struct_type_id = self.store.new_known(struct_type);

            // Update field type IDs with resolved types
            // Note: This is a bit awkward because we're modifying the original struct
            // For now, we'll rely on the annotation for type resolution during codegen
        }
        Ok(())
    }

    /// Infer types for a function definition
    fn infer_function(&mut self, func: &mut Function) -> CompileResult<()> {
        // Infer parameter types (fresh unknowns if unannotated)
        for param in &mut func.params {
            param.ty = if let Some(ann) = &param.annotation {
                self.store.new_known(ann.clone())
            } else {
                self.store.new_unknown()
            };
            self.symbols.define_var(&param.name, param.ty);
        }

        // Infer return type
        func.inferred_return = if let Some(ann) = &func.return_type {
            Some(self.store.new_known(ann.clone()))
        } else {
            Some(self.store.new_unknown())
        };

        self.current_return_type = func.inferred_return;

        // Infer function body
        self.infer_block(&mut func.body)?;

        self.current_return_type = None;

        // Register function signature in symbol table
        if let Some(ret_ty) = func.inferred_return {
            let param_tys: Vec<TypeId> = func.params.iter().map(|p| p.ty).collect();
            self.symbols.define_function(&func.name, param_tys, ret_ty);
        }

        Ok(())
    }

    /// Infer types for a block of statements
    fn infer_block(&mut self, block: &mut Block) -> CompileResult<()> {
        for stmt in &mut block.stmts {
            self.infer_stmt(stmt)?;
        }
        Ok(())
    }

    /// Infer types for a single statement
    fn infer_stmt(&mut self, stmt: &mut Stmt) -> CompileResult<()> {
        match stmt {
            Stmt::VarDecl {
                name,
                annotation,
                inferred,
                init,
            } => {
                if let Some(init_expr) = init {
                    let init_ty = self.infer_expr(init_expr)?;

                    *inferred = if let Some(ann) = annotation {
                        let ann_ty = self.store.new_known(ann.clone());
                        self.unify(ann_ty, init_ty)?;
                        ann_ty
                    } else {
                        init_ty
                    };

                    self.symbols.define_var(name, *inferred);
                } else if let Some(ann) = annotation {
                    // Declaration without initializer -> just use annotation
                    *inferred = self.store.new_known(ann.clone());
                    self.symbols.define_var(name, *inferred);
                } else {
                    return Err(CompilationError::TypeError(format!(
                        "Variable '{name}' declared without type annotation or initializer",
                    )));
                }
            }

            Stmt::Expr(expr) => {
                self.infer_expr(expr)?;
            }

            Stmt::Return(Some(expr)) => {
                let expr_ty = self.infer_expr(expr)?;
                if let Some(ret_ty) = self.current_return_type {
                    self.unify(ret_ty, expr_ty)?;
                } else {
                    return Err(CompilationError::TypeError(
                        "Return statement outside of function".into(),
                    ));
                }
            }

            // Void return - check if function expects void
            Stmt::Return(None) => {
                if let Some(ret_ty) = self.current_return_type {
                    let void_ty = self.store.new_known(Type::Void);
                    self.unify(ret_ty, void_ty)?;
                } else {
                    return Err(CompilationError::TypeError(
                        "Return statement outside of function".into(),
                    ));
                }
            }

            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_ty = self.infer_expr(cond)?;
                let bool_ty = self.store.new_known(Type::Bool);
                self.unify(cond_ty, bool_ty)?;

                self.infer_block(then_branch)?;
                if let Some(else_b) = else_branch {
                    self.infer_block(else_b)?;
                }
            }

            Stmt::While { cond, body } => {
                let cond_ty = self.infer_expr(cond)?;
                let bool_ty = self.store.new_known(Type::Bool);
                self.unify(cond_ty, bool_ty)?;

                self.infer_block(body)?;
            }

            Stmt::For { var, iter, body } => {
                let iter_ty = self.infer_expr(iter)?;

                // For i in N: iter should be Int-like OR a range (which we currently typed as Void)
                // TODO: Better range typing
                let iter_resolved = self
                    .store
                    .resolve(iter_ty)
                    .map_err(CompilationError::TypeError)?;
                if iter_resolved != Type::Int && iter_resolved != Type::Void {
                    return Err(CompilationError::TypeError(format!(
                        "For loop iterator must be Int or Range, found {iter_resolved:?}"
                    )));
                }

                let var_ty = self.store.new_known(Type::Int);
                self.symbols.define_var(var, var_ty);

                self.infer_block(body)?;
            }

            Stmt::Break | Stmt::Continue => {
                // No type inference needed
            }
        }
        Ok(())
    }

    /// Infer types for an expression
    fn infer_expr(&mut self, expr: &mut Expr) -> Result<TypeId, CompilationError> {
        let ty = match expr {
            Expr::Identifier(name, ty_id) => {
                let var_ty = self.symbols.lookup_var(name).ok_or_else(|| {
                    CompilationError::TypeError(format!("Use of undeclared variable '{name}'"))
                })?;
                *ty_id = var_ty;
                var_ty
            }

            Expr::Literal(lit, ty_id) => {
                let ty = match lit {
                    Literal::Int(_) => Type::Int,
                    Literal::Float(_) => Type::Float,
                    Literal::Bool(_) => Type::Bool,
                    Literal::String(_) => Type::CString,
                    Literal::Null => Type::Ptr(Box::new(Type::Void)),
                };
                let type_id = self.store.new_known(ty);
                *ty_id = type_id;
                type_id
            }

            Expr::Call { callee, args, ty } => {
                let (param_tys, ret_ty) = if let Some(sig) = self.symbols.lookup_function(callee) {
                    sig
                } else {
                    // For undeclared functions (like printf), we allow them but can't check params.
                    // We assume they return Void for now, or we could return a fresh unknown.
                    let void_ty = self.store.new_known(Type::Void);
                    (vec![], void_ty)
                };

                if !param_tys.is_empty() && args.len() != param_tys.len() {
                    return Err(CompilationError::TypeError(format!(
                        "Function '{}' expects {} arguments, got {}",
                        callee,
                        param_tys.len(),
                        args.len()
                    )));
                }

                if param_tys.is_empty() {
                    // Just infer arguments without unifying if signature is unknown (variadic C funcs)
                    for arg in args.iter_mut() {
                        self.infer_expr(arg)?;
                    }
                } else {
                    for (arg, param_ty) in args.iter_mut().zip(param_tys.iter()) {
                        let arg_ty = self.infer_expr(arg)?;
                        self.unify(arg_ty, *param_ty)?;
                    }
                }

                *ty = ret_ty;
                ret_ty
            }

            Expr::UnaryOp { op, expr, ty } => {
                let expr_ty = self.infer_expr(expr)?;

                // Unary operators typically preserve type (except address-of)
                let result_ty = match op {
                    UnaryOperator::AddressOf => {
                        let resolved = self
                            .store
                            .resolve(expr_ty)
                            .map_err(CompilationError::TypeError)?;
                        let ptr_ty = Type::Ptr(Box::new(resolved));
                        self.store.new_known(ptr_ty)
                    }
                    UnaryOperator::Dereference => {
                        let resolved = self
                            .store
                            .resolve(expr_ty)
                            .map_err(CompilationError::TypeError)?;
                        if let Type::Ptr(inner_ty) = resolved {
                            self.store.new_known(*inner_ty)
                        } else {
                            return Err(CompilationError::TypeError(format!(
                                "Cannot dereference non-pointer type: {resolved:?}"
                            )));
                        }
                    }
                    _ => expr_ty,
                };

                *ty = result_ty;
                result_ty
            }

            Expr::BinaryOp {
                op,
                left,
                right,
                ty,
            } => {
                let left_ty = self.infer_expr(left)?;
                let right_ty = self.infer_expr(right)?;

                // Result type depends on operator
                let result_ty = match op {
                    BinaryOperator::And | BinaryOperator::Or => {
                        let bool_ty = self.store.new_known(Type::Bool);
                        self.unify(left_ty, bool_ty)?;
                        self.unify(right_ty, bool_ty)?;
                        bool_ty
                    }
                    BinaryOperator::Eq
                    | BinaryOperator::Ne
                    | BinaryOperator::Lt
                    | BinaryOperator::Gt
                    | BinaryOperator::Le
                    | BinaryOperator::Ge => {
                        self.unify(left_ty, right_ty)?;
                        self.store.new_known(Type::Bool)
                    }
                    _ => {
                        self.unify(left_ty, right_ty)?;
                        left_ty
                    }
                };

                *ty = result_ty;
                result_ty
            }

            Expr::Assign {
                op: _,
                left,
                right,
                ty,
            } => {
                let right_ty = self.infer_expr(right)?;
                let left_ty = self.infer_expr(left)?;

                // Assignment: right must unify with left
                self.unify(left_ty, right_ty)?;

                *ty = left_ty;
                left_ty
            }

            Expr::If {
                cond,
                then_branch,
                else_branch,
                ty,
            } => {
                let cond_ty = self.infer_expr(cond)?;
                let bool_ty = self.store.new_known(Type::Bool);
                self.unify(cond_ty, bool_ty)?;

                let then_ty = self.infer_expr(then_branch)?;
                let else_ty = self.infer_expr(else_branch)?;

                self.unify(then_ty, else_ty)?;

                *ty = then_ty;
                then_ty
            }

            Expr::RangeLiteral { start, end } => {
                // Range literals are only used in for loop context
                let start_ty = self.infer_expr(start)?;
                let end_ty = self.infer_expr(end)?;

                let int_ty = self.store.new_known(Type::Int);
                self.unify(start_ty, int_ty)?;
                self.unify(end_ty, int_ty)?;

                // Return a dummy type -> ranges don't have their own type
                self.store.new_known(Type::Void)
            }
        };

        Ok(ty)
    }

    /// Unify two type IDs
    fn unify(&mut self, a: TypeId, b: TypeId) -> CompileResult<()> {
        self.store.unify(a, b).map_err(CompilationError::TypeError)
    }
}
