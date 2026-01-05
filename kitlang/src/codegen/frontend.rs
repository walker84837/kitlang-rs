use crate::{KitParser, Rule, error::CompilationError};
use pest::Parser;

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::codegen::ast::{Include, Program, Function, Block, Stmt, Expr, Literal};
use crate::codegen::compiler::{CompilerMeta, CompilerOptions, Toolchain};
use crate::codegen::types::{Type, ToCRepr};
use crate::codegen::parser::Parser as CodeParser;

pub type CompileResult<T> = Result<T, CompilationError>;

pub struct Compiler {
    files: Vec<PathBuf>,
    output: PathBuf,
    c_output: PathBuf,
    includes: Vec<Include>,
    libs: Vec<String>,
    parser: CodeParser,
}

impl Compiler {
    pub fn new(files: Vec<PathBuf>, output: impl AsRef<Path>, libs: Vec<String>) -> Self {
        Self {
            files,
            output: output.as_ref().to_path_buf(),
            c_output: output.as_ref().with_extension("c"),
            includes: Vec::new(),
            libs,
            parser: CodeParser,
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

    fn transpile_with_program(&mut self, prog: Program) {
        let c_code = self.generate_c_code(prog);
        if let Err(e) = std::fs::write(&self.c_output, c_code) {
            panic!("Failed to write output: {}", e);
        }
    }

    fn generate_c_code(&self, prog: Program) -> String {
        let mut out = String::new();

        // emit regular includes from the source `include` statements
        for inc in &prog.includes {
            // Emit as #include "path" as per C preprocessor rules for Kit includes.
            let line = format!("#include \"{}\"\n", inc.path);
            out.push_str(&line);
        }

        let mut seen_headers = HashSet::new();
        // Vec preserves order
        let mut seen_declarations = Vec::new();

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
            if let Some(r) = &func.return_type {
                collect_from_type(r);
            }
            for p in &func.params {
                collect_from_type(&p.ty);
            }
            for stmt in &func.body.stmts {
                if let Stmt::VarDecl { ty: Some(t), .. } = stmt {
                    collect_from_type(t);
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
        out.push('\n');

        // emit functions as before...
        for func in prog.functions {
            out.push_str(&self.transpile_function(&func));
            out.push_str("\n\n");
        }
        out
    }

    fn transpile_function(&self, func: &Function) -> String {
        let return_type = if func.name == "main" {
            "int".to_string()
        } else {
            func.return_type
                .as_ref()
                .map_or("void".to_string(), |t| self.transpile_type(t))
        };

        let params = func
            .params
            .iter()
            .map(|p| format!("{} {}", self.transpile_type(&p.ty), p.name))
            .collect::<Vec<_>>()
            .join(", ");

        let mut body = self.transpile_block(&func.body);

        if func.name == "main" {
            let has_return = func
                .body
                .stmts
                .iter()
                .any(|stmt| matches!(stmt, Stmt::Return(_)));
            if !has_return {
                body.push_str("    return 0;\n");
            }
        }

        format!("{} {}({}) {{\n{}}}", return_type, func.name, params, body)
    }

    fn transpile_block(&self, block: &Block) -> String {
        let mut code = String::new();
        for stmt in &block.stmts {
            match stmt {
                Stmt::VarDecl { name, ty, init } => {
                    let ty_str = if let Some(t) = ty {
                        self.transpile_type(t)
                    } else if let Some(Expr::Literal(Literal::Int(_))) = init {
                        "int".to_string()
                    } else {
                        "auto".to_string()
                    };
                    match init {
                        Some(expr) => {
                            let init_str = self.transpile_expr(expr);
                            code.push_str(&format!("{} {} = {};\n", ty_str, name, init_str));
                        }
                        None => {
                            code.push_str(&format!("{} {};\n", ty_str, name));
                        }
                    }
                }
                Stmt::Expr(expr) => {
                    code.push_str(&self.transpile_expr(expr));
                    code.push_str(";\n");
                }
                // TODO: should add a return to the main function anyway
                Stmt::Return(expr) => {
                    let expr_str = expr
                        .as_ref()
                        .map_or(String::new(), |e| format!(" {}", self.transpile_expr(e)));
                    code.push_str(&format!("return{};\n", expr_str));
                }
                Stmt::If {
                    cond,
                    then_branch,
                    else_branch,
                } => {
                    let cond_str = self.transpile_expr(cond);
                    let then_code = self.transpile_block(then_branch);
                    let mut if_str = format!("if ({}) {{\n{}}}", cond_str, then_code);
                    if let Some(else_b) = else_branch {
                        let else_code = self.transpile_block(else_b);
                        if_str.push_str(&format!(" else {{\n{}}}", else_code));
                    }
                    code.push_str(&if_str);
                }
                Stmt::While { cond, body } => {
                    let cond_str = self.transpile_expr(cond);
                    let body_code = self.transpile_block(body);
                    code.push_str(&format!("while ({}) {{\n{}}}", cond_str, body_code));
                }
                // Translate `for i in 10` to `for (int i = 0; i < 10; ++i)`
                // Of course, this assumes `iter` (i) is an integer literal or expression that evaluates to an integer.
                Stmt::For { var, iter, body } => {
                    let body_code = self.transpile_block(body);
                    match iter {
                        Expr::RangeLiteral { start, end } => {
                            // Handle range literals: `for i in 1...10`
                            let start_str = self.transpile_expr(start);
                            let end_str = self.transpile_expr(end);
                            code.push_str(&format!(
                                "for (int {} = {}; {} < {}; ++{}) {{\n{}}}",
                                var, start_str, var, end_str, var, body_code
                            ));
                        }
                        _ => {
                            // Handle single integer expressions: `for i in 3`
                            let iter_str = self.transpile_expr(iter);
                            code.push_str(&format!(
                                "for (int {} = 0; {} < {}; ++{}) {{\n{}}}",
                                var, var, iter_str, var, body_code
                            ));
                        }
                    }
                }
                Stmt::Break => {
                    code.push_str("break;\n");
                }
                Stmt::Continue => {
                    code.push_str("continue;\n");
                }
            }
        }
        code
    }

    fn transpile_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Identifier(id) => id.clone(),
            Expr::Literal(lit) => lit.to_c(),
            Expr::Call { callee, args } => {
                let args_str = args
                    .iter()
                    .map(|a| self.transpile_expr(a))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", callee, args_str)
            }
            Expr::UnaryOp { op, expr } => {
                let expr_str = self.transpile_expr(expr);
                op.to_string_with_expr(expr_str)
            }
            Expr::BinaryOp { op, left, right } => {
                let left_str = self.transpile_expr(left);
                let right_str = self.transpile_expr(right);
                format!("({} {} {})", left_str, op.to_c_str(), right_str)
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_str = self.transpile_expr(cond);
                let then_str = self.transpile_expr(then_branch);
                let else_str = self.transpile_expr(else_branch);
                format!("({}) ? ({}) : ({})", cond_str, then_str, else_str)
            }
            Expr::Assign { op, left, right } => {
                let left_str = self.transpile_expr(left);
                let right_str = self.transpile_expr(right);
                format!("({} {} {})", left_str, op.to_c_str(), right_str)
            }
            Expr::RangeLiteral { start: _, end: _ } => {
                // Range literals are not directly transpiled to C
                // They are only used in for loop context
                panic!("Range literals should only be used in for loop expressions")
            }
        }
    }

    fn transpile_type(&self, ty: &Type) -> String {
        ty.to_c_repr().name
    }

    pub fn compile(&mut self) -> CompileResult<()> {
        let prog = self.parse()?;
        self.transpile_with_program(prog);

        let detected = Toolchain::executable_path().ok_or(CompilationError::ToolchainNotFound)?;

        // FIX: Handle non-UTF-8 paths
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
