use crate::{
    KitParser, Rule,
    codegen::{
        compiler::{self, CompilerMeta, CompilerOptions, Toolchain},
        types::*,
    },
    error::CompilationError,
};
use log::debug;

use pest::{Parser, iterators::Pair};

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

type CompileResult<T> = Result<T, CompilationError>;

pub enum Optimizations {
    // Should be handled by the compiler itself
    Simple = 1,
    Medium = 2,
    Aggressive = 3,
}

pub struct Compiler {
    files: Vec<PathBuf>,
    output: PathBuf,
    includes: Vec<Include>,
    libs: Vec<String>,
}

/// Returns None when the string is not escaped
// TODO: this might be useful in other places
#[allow(dead_code)]
fn unescape(s: impl AsRef<str>) -> Option<String> {
    let s = s.as_ref();
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next()? {
                'n' => out.push('\n'),
                'r' => out.push('\r'),
                't' => out.push('\t'),
                '\\' => out.push('\\'),
                '\'' => out.push('\''),
                '"' => out.push('"'),
                // For this grammar, `\` escapes any character
                other => out.push(other),
            }
        } else {
            out.push(c);
        }
    }
    Some(out)
}

impl Compiler {
    pub fn new(files: Vec<PathBuf>, output: impl AsRef<Path>, libs: Vec<String>) -> Self {
        Self {
            files,
            output: output.as_ref().to_path_buf(),
            includes: Vec::new(),
            libs,
        }
    }

    fn parse(&mut self) -> CompileResult<Program> {
        let mut includes = Vec::new();
        let mut functions = Vec::new();

        for file in &self.files {
            let input = std::fs::read_to_string(file).map_err(CompilationError::Io)?;

            let pairs = KitParser::parse(Rule::program, &input)
                .map_err(|e| CompilationError::ParseError(e.to_string()))?;

            for pair in pairs {
                match pair.as_rule() {
                    Rule::include_stmt => includes.push(self.parse_include(pair)),
                    Rule::function_decl => functions.push(self.parse_function(pair)?), // Propagate error
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

    fn parse_include(&self, pair: Pair<Rule>) -> Include {
        // include_stmt = { "include" ~ string ~ ("=>" ~ string)? ~ ";" }
        let mut inner = pair.into_inner();
        // SAFETY: Grammar guarantees at least one child (the string literal)
        let lit = inner.next().unwrap().as_str();

        // lit includes the quotes, so strip them
        let path = lit[1..lit.len() - 1].to_string();
        Include { path }
    }

    fn parse_function(&self, pair: Pair<Rule>) -> CompileResult<Function> {
        let mut inner = pair.into_inner();

        // SAFETY: Grammar guarantees function name exists as first child
        let name = inner.next().unwrap().as_str().to_string();

        let mut params: Vec<Param> = Vec::new();
        let mut return_type: Option<Type> = None;
        let mut body: Block = Block { stmts: Vec::new() };

        // consume remaining child nodes and match on their rules
        for node in inner {
            match node.as_rule() {
                Rule::params => {
                    params = self.parse_params(node)?;
                }
                Rule::type_annotation => {
                    return_type = Some(self.parse_type(node)?);
                }
                Rule::block => {
                    body = self.parse_block(node)?;
                }
                _ => {
                    // skip punctuation/other tokens
                }
            }
        }

        Ok(Function {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_params(&self, pair: Pair<Rule>) -> CompileResult<Vec<Param>> {
        // param_list = { param ~ ("," ~ param )* }
        pair.into_inner()
            .filter(|p: &Pair<Rule>| p.as_rule() == Rule::param)
            .map(|p: Pair<Rule>| {
                let mut inner = p.into_inner();
                // SAFETY: Grammar guarantees param has identifier and type
                let name = inner.next().unwrap().as_str().to_string();
                let type_node = inner.next().unwrap();
                let ty = self.parse_type(type_node)?;
                Ok(Param { name, ty })
            })
            .collect()
    }

    fn parse_block(&self, pair: Pair<Rule>) -> CompileResult<Block> {
        // block = { "{" ~ (statement)* ~ "}" }
        let stmts = pair
            .into_inner()
            // grammar gives us a wrapper Rule::statement
            .filter(|p: &Pair<Rule>| p.as_rule() == Rule::statement)
            .map(|stmt_pair: Pair<Rule>| {
                // SAFETY: Grammar guarantees exactly one child in statement wrapper
                let inner = stmt_pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::var_decl => self.parse_var_decl(inner),
                    Rule::expr_stmt => self.parse_expr_stmt(inner),
                    Rule::return_stmt => self.parse_return(inner),
                    other => Err(CompilationError::ParseError(format!(
                        "unexpected statement: {:?}",
                        other
                    ))),
                }
            })
            .collect::<Result<_, _>>()?; // Collect and propagate errors
        Ok(Block { stmts })
    }

    fn parse_var_decl(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // var_decl = { ("var"|"const") ~ ident ~ (":" ~ type_annotation)? ~ ("=" ~ expr)? ~ ";" }
        let mut name: Option<String> = None;
        let mut ty: Option<Type> = None;
        let mut init: Option<Expr> = None;

        for child in pair.into_inner() {
            match child.as_rule() {
                Rule::identifier if name.is_none() => {
                    // first identifier is the var name
                    name = Some(child.as_str().to_string());
                }
                Rule::type_annotation => {
                    // e.g. ": CString"
                    ty = Some(self.parse_type(child)?);
                }
                Rule::expr => {
                    init = Some(self.parse_expr(child)?);
                }
                _ => { /* skip punctuation and the 'var'/'const' keyword */ }
            }
        }

        let name = name.ok_or(CompilationError::ParseError(
            "var_decl missing identifier".to_string(),
        ))?;
        Ok(Stmt::VarDecl { name, ty, init })
    }

    fn parse_type(&self, pair: Pair<Rule>) -> CompileResult<Type> {
        let mut inner = pair.into_inner();
        // SAFETY: Grammar guarantees at least base type exists
        let base = inner.next().unwrap().as_str().trim();
        // turn known names into their enum cases
        let mut ty = Type::from_kit(base);

        // if there are array or pointer annotations, handle here…
        for sub in inner {
            // you can extend to multi-dimensional or pointer types
            let inner_ty = self.parse_type(sub)?;
            ty = Type::Ptr(Box::new(inner_ty));
        }
        Ok(ty)
    }

    fn transpile_with_program(&mut self, prog: Program) {
        let c_code = self.generate_c_code(prog);
        if let Err(e) = std::fs::write(&self.output, c_code) {
            panic!("Failed to write output: {}", e);
        }
    }

    fn parse_expr_stmt(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // expr_stmt = { expr ~ ";" }
        // SAFETY: Grammar guarantees expression exists as first child
        let expr_pair = pair.into_inner().next().unwrap();
        Ok(Stmt::Expr(self.parse_expr(expr_pair)?))
    }

    fn parse_return(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // return_stmt = { "return" ~ expr? ~ ";" }
        let mut inner = pair.into_inner();
        let expr = inner.next().map(|p| self.parse_expr(p)).transpose()?;
        Ok(Stmt::Return(expr))
    }

    fn parse_expr(&self, pair: Pair<Rule>) -> CompileResult<Expr> {
        // first, peel off any of the wrapper rules:
        match pair.as_rule() {
            Rule::expr
            | Rule::assign
            | Rule::logical_or
            | Rule::logical_and
            | Rule::equality
            | Rule::comparison
            | Rule::additive
            | Rule::multiplicative
            | Rule::bitwise_or
            | Rule::bitwise_xor
            | Rule::bitwise_and
            | Rule::shift => {
                // SAFETY: Grammar guarantees exactly one child for wrapper rules
                let inner = pair.into_inner().next().unwrap();
                return self.parse_expr(inner);
            }
            _ => { /* Do nothing here, fall through to the next match */ }
        }

        // now handle the real terminals
        match pair.as_rule() {
            Rule::unary => {
                let mut inner_pairs = pair.into_inner();
                // SAFETY: Unary rule always has at least one child
                let first_pair = inner_pairs.next().unwrap();

                match first_pair.as_rule() {
                    Rule::unary_op => {
                        let op_str = first_pair.as_str();
                        let op = UnaryOperator::from_str(op_str).map_err(|_| {
                            CompilationError::ParseError(format!(
                                "invalid unary operation: {}",
                                op_str
                            ))
                        })?;

                        // SAFETY: Grammar guarantees expression after unary op
                        let expr = self.parse_expr(inner_pairs.next().unwrap())?;
                        Ok(Expr::UnaryOp {
                            op,
                            expr: Box::new(expr),
                        })
                    }
                    Rule::primary => self.parse_expr(first_pair),
                    other => Err(CompilationError::ParseError(format!(
                        "Unexpected rule in unary: {:?}",
                        other
                    ))),
                }
            }
            Rule::identifier => Ok(Expr::Identifier(pair.as_str().to_string())),
            Rule::literal => {
                // SAFETY: Grammar guarantees exactly one child in literal
                let inner = pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::number => {
                        // SAFETY: Number always has exactly one child (integer/float)
                        let num_pair = inner.into_inner().next().unwrap();
                        match num_pair.as_rule() {
                            Rule::integer => {
                                let s = num_pair.as_str();
                                let i = s.parse::<i64>().map_err(|e| {
                                    CompilationError::ParseError(format!(
                                        "invalid integer literal '{}': {}",
                                        s, e
                                    ))
                                })?;
                                Ok(Expr::Literal(Literal::Int(i)))
                            }
                            Rule::float => {
                                let s = num_pair.as_str();
                                let f = s.parse::<f64>().map_err(|e| {
                                    CompilationError::ParseError(format!(
                                        "invalid float literal '{}': {}",
                                        s, e
                                    ))
                                })?;
                                Ok(Expr::Literal(Literal::Float(f)))
                            }
                            _ => Err(CompilationError::ParseError(
                                "Unexpected number type".to_string(),
                            )),
                        }
                    }
                    Rule::boolean => match inner.as_str() {
                        "true" => Ok(Expr::Literal(Literal::Bool(true))),
                        "false" => Ok(Expr::Literal(Literal::Bool(false))),
                        s => Err(CompilationError::ParseError(format!(
                            "invalid boolean literal: {}",
                            s
                        ))),
                    },
                    Rule::char_literal => Err(CompilationError::ParseError(
                        "char literal parsing not implemented".to_string(),
                    )),
                    _ => Err(CompilationError::ParseError(format!(
                        "Unexpected literal type: {:?}",
                        inner.as_rule()
                    ))),
                }
            }
            Rule::string => {
                let full = pair.as_str();
                let inner = &full[1..full.len() - 1];
                let unescaped = unescape(inner).unwrap_or_else(|| inner.to_string());
                Ok(Expr::Literal(Literal::String(unescaped)))
            }
            Rule::function_call_expr => {
                let mut inner = pair.into_inner();
                // SAFETY: Grammar guarantees callee identifier exists
                let callee = inner.next().unwrap().as_str().to_string();
                let args = inner
                    .filter(|p: &Pair<Rule>| p.as_rule() == Rule::expr)
                    .map(|p: Pair<Rule>| self.parse_expr(p))
                    .collect::<Result<Vec<_>, _>>()?; // Collect and propagate errors
                Ok(Expr::Call { callee, args })
            }
            Rule::primary => {
                // SAFETY: Primary rule always has exactly one child
                let inner = pair.into_inner().next().unwrap();
                self.parse_expr(inner)
            }
            other => Err(CompilationError::ParseError(format!(
                "Unexpected expr rule: {:?}",
                other
            ))),
        }
    }

    fn generate_c_code(&self, prog: Program) -> String {
        let mut out = String::new();

        // emit regular includes from the source `include` statements
        for inc in &prog.includes {
            let line = if inc.path.starts_with('<') || inc.path.ends_with(".h>") {
                format!("#include {}\n", inc.path)
            } else {
                format!("#include \"{}\"\n", inc.path.trim_matches('"'))
            };
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
                .map(|t| self.transpile_type(t))
                .unwrap_or_else(|| "void".to_string())
        };

        debug!("return type: {}", return_type);

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
                    let ty_str = ty
                        .as_ref()
                        .map_or("auto".to_string(), |t| self.transpile_type(t));
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
            }
        }
        code
    }

    fn transpile_expr(&self, expr: &Expr) -> String {
        debug!("Transpiling expr: {:#?}", expr);
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
        }
    }

    fn transpile_type(&self, ty: &Type) -> String {
        ty.to_c_repr().name
    }

    pub fn compile(&mut self) -> CompileResult<()> {
        let prog = self.parse()?;
        self.transpile_with_program(prog);

        let out_c = if self.output.extension().unwrap_or_default() == "c" {
            self.output.clone()
        } else {
            // ensure .c extension
            self.output.with_extension("c")
        };

        // FIX: Handle path errors properly
        let exe_stem = self
            .output
            .file_stem()
            .ok_or(CompilationError::InvalidOutputPath)?;
        let exe_name = exe_stem
            .to_str()
            .ok_or(CompilationError::InvalidOutputPath)?
            .to_string();

        let detected =
            compiler::get_system_compiler().ok_or(CompilationError::ToolchainNotFound)?;

        // FIX: Handle non-UTF-8 paths
        let target_path = out_c
            .clone()
            .into_os_string()
            .into_string()
            .map_err(|_| CompilationError::InvalidOutputPath)?;

        let opts = CompilerOptions::new(CompilerMeta(detected.0))
            .link_libs(&self.libs)
            .lib_paths(&["/usr/local/lib"])
            .targets(&[target_path])
            .build();

        let mut cmd = Command::new(&detected.1);
        cmd.arg(out_c);

        let exe_name_with_ext = if detected.0.is_msvc() {
            format!("{}.exe", exe_name)
        } else {
            exe_name
        };

        match detected.0 {
            Toolchain::Gcc | Toolchain::Clang => {
                cmd.arg("-o").arg(&exe_name_with_ext);
            }
            #[cfg(windows)]
            Toolchain::Msvc => {
                cmd.arg(format!("/Fe:{}", exe_name_with_ext));
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
            return Err(CompilationError::CCompileError(output.stderr));
        }

        Ok(())
    }
}
