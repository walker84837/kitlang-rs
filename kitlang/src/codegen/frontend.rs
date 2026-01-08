use crate::{KitParser, Rule, error::CompilationError};
use crate::error::CompileResult;
use pest::Parser;

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::codegen::ast::*;
use crate::codegen::compiler::{CompilerMeta, CompilerOptions, Toolchain};
use crate::codegen::inference::TypeInferencer;
use crate::codegen::parser::Parser as CodeParser;
use crate::codegen::types::{ToCRepr, Type};

pub struct Compiler {
    files: Vec<PathBuf>,
    output: PathBuf,
    c_output: PathBuf,
    includes: Vec<Include>,
    libs: Vec<String>,
    parser: CodeParser,
    inferencer: TypeInferencer,
}

impl Compiler {
    pub fn new(files: Vec<PathBuf>, output: impl AsRef<Path>, libs: Vec<String>) -> Self {
        Self {
            files,
            output: output.as_ref().to_path_buf(),
            c_output: output.as_ref().with_extension("c"),
            includes: Vec::new(),
            libs,
            parser: CodeParser::new(),
            inferencer: TypeInferencer::new(),
        }
    }

    fn parse(&mut self) -> CompileResult<Program> {
        let mut includes = Vec::new();
        let mut functions = Vec::new();

        // TODO: track which files are UTF-8 formatted:
        // - true = UTF-8
        // - false = binary (NOT ACCEPTED)
        // each files correspond to an index in the `files` vector
        let _files = vec![false; self.files.len()];

        for file in &self.files {
            let input = std::fs::read_to_string(file).map_err(CompilationError::Io)?;

            let pairs = KitParser::parse(Rule::program, &input)
                .map_err(|e| CompilationError::ParseError(e.to_string()))?;

            for pair in pairs {
                match pair.as_rule() {
                    Rule::include_stmt => includes.push(self.parser.parse_include(pair)),
                    Rule::function_decl => functions.push(self.parser.parse_function(pair)?), // Propagate error
                    _ => {}
                }
            }
        }

        self.includes = includes.clone();

        Ok(Program {
            includes,
            imports: HashSet::new(),
            functions,
        })
    }

    /// Generate C code from the AST and write it to the output path
    fn transpile_with_program(&mut self, prog: Program) {
        let c_code = self.generate_c_code(&prog);
        if let Err(e) = std::fs::write(&self.c_output, c_code) {
            panic!("Failed to write output: {}", e);
        }
    }

    fn generate_c_code(&self, prog: &Program) -> String {
        let mut out = String::new();

        // emit regular includes from the source `include` statements
        for inc in &prog.includes {
            // Emit as #include "path" as per C preprocessor rules for Kit includes.
            let line = format!("#include \"{}\"\n", inc.path);
            out.push_str(&line);
        }

        let mut seen_headers = HashSet::new();
        // Vec preserves order
        let mut seen_declarations: Vec<String> = Vec::new();

        let mut collect_from_type = |t: &Type| {
            let ctype = t.to_c_repr();
            for h in ctype.headers {
                seen_headers.insert(h);
            }
            if let Some(d) = ctype.declaration
                && !seen_declarations.contains(&d)
            {
                seen_declarations.push(d);
            }
        };

        // scan every function signature & body for types to gather their headers/typedefs
        for func in &prog.functions {
            // Use inferred return type
            if let Some(ret_id) = func.inferred_return {
                if let Ok(ty) = self.inferencer.store.resolve(ret_id) {
                    collect_from_type(&ty);
                }
            } else if let Some(r) = &func.return_type {
                collect_from_type(r);
            }

            for p in &func.params {
                // Use inferred param type
                if let Ok(ty) = self.inferencer.store.resolve(p.ty) {
                    collect_from_type(&ty);
                } else if let Some(ann) = &p.annotation {
                    collect_from_type(ann);
                }
            }

            for stmt in &func.body.stmts {
                if let Stmt::VarDecl { inferred, .. } = stmt
                    && let Ok(ty) = self.inferencer.store.resolve(*inferred)
                {
                    collect_from_type(&ty);
                }
            }
        }

        // emit unique headers
        for hdr in seen_headers {
            out.push_str(&format!("#include {}\n", hdr));
        }
        out.push('\n');

        // emit each unique typedef
        for decl in seen_declarations {
            out.push_str(&decl);
            out.push('\n');
        }

        // emit functions as before...
        for func in &prog.functions {
            out.push_str(&self.transpile_function(func));
            out.push_str("\n\n");
        }
        out
    }

    fn transpile_function(&self, func: &Function) -> String {
        let return_type = if func.name == "main" {
            "int".to_string()
        } else {
            // Try inferred return type first
            func.inferred_return
                .and_then(|id| self.inferencer.store.resolve(id).ok())
                .map(|t| t.to_c_repr().name)
                .or_else(|| func.return_type.as_ref().map(|t| t.to_c_repr().name))
                .unwrap_or_else(|| "void".to_string())
        };

        let params = func
            .params
            .iter()
            .map(|p| {
                let ty_name = self
                    .inferencer
                    .store
                    .resolve(p.ty)
                    .map(|t| t.to_c_repr().name)
                    .or_else(|_| p.annotation.as_ref().map(|t| t.to_c_repr().name).ok_or(()))
                    .unwrap_or("void*".to_string()); // Fallback
                format!("{} {}", ty_name, p.name)
            })
            .collect::<Vec<_>>()
            .join(", ");

        let mut body_code = self.transpile_block(&func.body);

        if func.name == "main" {
            let has_return = func
                .body
                .stmts
                .iter()
                .any(|stmt| matches!(stmt, Stmt::Return(_)));
            if !has_return {
                // Insert return 0 before the closing brace
                if let Some(pos) = body_code.rfind('}') {
                    body_code.insert_str(pos, "    return 0;\n");
                }
            }
        }

        format!("{} {}({}) {}", return_type, func.name, params, body_code)
    }

    fn transpile_block(&self, block: &Block) -> String {
        let mut code = String::from("{\n");
        for stmt in &block.stmts {
            let stmt_code = match stmt {
                Stmt::VarDecl {
                    name,
                    annotation: _,
                    inferred,
                    init,
                } => {
                    let ty_str = self
                        .inferencer
                        .store
                        .resolve(*inferred)
                        .map(|t| t.to_c_repr().name)
                        .unwrap_or_else(|_| "auto".to_string());

                    match init {
                        Some(expr) => {
                            let init_str = self.transpile_expr(expr);
                            format!("{} {} = {};\n", ty_str, name, init_str)
                        }
                        None => {
                            format!("{} {};\n", ty_str, name)
                        }
                    }
                }
                Stmt::Expr(expr) => {
                    format!("{};\n", self.transpile_expr(expr))
                }
                Stmt::Return(expr) => {
                    if let Some(e) = expr {
                        format!("return {};\n", self.transpile_expr(e))
                    } else {
                        "return;\n".to_string()
                    }
                }
                Stmt::If {
                    cond,
                    then_branch,
                    else_branch,
                } => {
                    let mut s = format!("if ({}) ", self.transpile_expr(cond));
                    s.push_str(&self.transpile_block(then_branch));
                    if let Some(else_b) = else_branch {
                        s.push_str(" else ");
                        s.push_str(&self.transpile_block(else_b));
                    }
                    s.push('\n');
                    s
                }
                Stmt::While { cond, body } => {
                    let mut s = format!("while ({}) ", self.transpile_expr(cond));
                    s.push_str(&self.transpile_block(body));
                    s.push('\n');
                    s
                }
                Stmt::For { var, iter, body } => {
                    let mut s = if let Expr::RangeLiteral { start, end } = iter {
                        let start_str = self.transpile_expr(start);
                        let end_str = self.transpile_expr(end);
                        format!(
                            "for (int {} = {}; {} < {}; ++{}) ",
                            var, start_str, var, end_str, var
                        )
                    } else {
                        let iter_str = self.transpile_expr(iter);
                        format!("for (int {} = 0; {} < {}; ++{}) ", var, var, iter_str, var)
                    };
                    s.push_str(&self.transpile_block(body));
                    s.push('\n');
                    s
                }
                Stmt::Break => "break;\n".to_string(),
                Stmt::Continue => "continue;\n".to_string(),
            };

            for line in stmt_code.lines() {
                code.push_str("    ");
                code.push_str(line);
                code.push('\n');
            }
        }
        code.push('}');
        code
    }

    fn transpile_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Identifier(name, _) => name.clone(),
            Expr::Literal(lit, _) => lit.to_c(),
            Expr::Call {
                callee,
                args,
                ty: _,
            } => {
                let args_str = args
                    .iter()
                    .map(|a| self.transpile_expr(a))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", callee, args_str)
            }
            Expr::UnaryOp { op, expr, ty: _ } => {
                let expr_str = self.transpile_expr(expr);
                format!("{}({})", op.to_c_str(), expr_str)
            }
            Expr::BinaryOp {
                op,
                left,
                right,
                ty: _,
            } => {
                let left_str = self.transpile_expr(left);
                let right_str = self.transpile_expr(right);
                format!("({} {} {})", left_str, op.to_c_str(), right_str)
            }
            Expr::Assign {
                op,
                left,
                right,
                ty: _,
            } => {
                let left_str = self.transpile_expr(left);
                let right_str = self.transpile_expr(right);
                format!("{} {} {}", left_str, op.to_c_str(), right_str)
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
                ty: _,
            } => {
                let cond_str = self.transpile_expr(cond);
                let then_str = self.transpile_expr(then_branch);
                let else_str = self.transpile_expr(else_branch);
                format!("({} ? {} : {})", cond_str, then_str, else_str)
            }
            Expr::RangeLiteral { .. } => {
                // Should technically not be used alone, but return something safe to avoid panic
                "/* range literal */ 0".to_string()
            }
        }
    }

    pub fn compile(&mut self) -> CompileResult<()> {
        let mut prog = self.parse()?;

        self.inferencer.infer_program(&mut prog)?;
        self.transpile_with_program(prog);

        let detected = Toolchain::executable_path().ok_or(CompilationError::ToolchainNotFound)?;

        // TODO: Handle non-UTF-8 paths
        let target_path = self
            .output
            .clone()
            .into_os_string()
            .into_string()
            .map_err(|_| CompilationError::InvalidOutputPath)?;

        let opts = CompilerOptions::new(CompilerMeta(detected.0))
            .link_libs(&self.libs)
            .lib_paths(&["/usr/local/lib"])
            .sources(&[&self.c_output])
            .output(&target_path)
            .build();

        let mut cmd = Command::new(&detected.1);

        // Get C99 compiler flags from the toolchain to make sure correct C standard based on
        // toolchain and include paths are used
        let compiler_flags = detected.0.get_compiler_flags();
        cmd.args(&compiler_flags);
        cmd.arg(&self.c_output);

        match detected.0 {
            Toolchain::Gcc | Toolchain::Clang => {
                cmd.arg("-o").arg(&self.output);
            }
            #[cfg(windows)]
            Toolchain::Msvc => {
                cmd.arg(format!("/Fe:{}", self.output.display()));
            }
            Toolchain::Other => {
                return Err(CompilationError::UnsupportedToolchain(
                    detected.1.display().to_string(),
                ));
            }
        }

        cmd.args(&opts.link_opts);

        let output = cmd.output().map_err(CompilationError::Io)?;
        let status = output.status;

        if !status.success() {
            // Keep the C source file for debugging.
            return Err(CompilationError::CCompileError(output.stderr));
        }

        // If we don't want to keep C source files, delete after compilation
        if std::env::var("KEEP_C").is_err()
            && let Err(err) = std::fs::remove_file(&self.c_output)
        {
            log::warn!(
                "Failed to remove intermediate C file {}: {err}",
                self.c_output.display()
            );
        }

        Ok(())
    }
}
