use crate::{
    KitParser, Rule,
    codegen::{
        compiler::{CompilerMeta, CompilerOptions, Toolchain},
        types::*,
    },
    error::CompilationError,
    parse_error,
};
use pest::{Parser, iterators::Pair};

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

type CompileResult<T> = Result<T, CompilationError>;

pub struct Compiler {
    files: Vec<PathBuf>,
    output: PathBuf,
    c_output: PathBuf,
    includes: Vec<Include>,
    libs: Vec<String>,
}

// TODO: this might be useful in other places
#[allow(dead_code)]
fn unescape(s: impl AsRef<str>) -> Option<String> {
    // TODO: search if there is an escape sequence beforehand to
    // avoid the allocation and search logic
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
            c_output: output.as_ref().with_extension("c"),
            includes: Vec::new(),
            libs,
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
        // The first child is the string literal representing the include path
        let path_literal_pair = inner.next().unwrap();
        let path_str = path_literal_pair.as_str();

        // Strip the quotes from the string literal
        let path = path_str[1..path_str.len() - 1].to_string();
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
                    Rule::if_stmt => self.parse_if_stmt(inner),
                    Rule::while_stmt => self.parse_while_stmt(inner),
                    Rule::for_stmt => self.parse_for_stmt(inner),
                    Rule::break_stmt => Ok(Stmt::Break),
                    Rule::continue_stmt => Ok(Stmt::Continue),
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

        let name = name.ok_or(parse_error!("var_decl missing identifier"))?;
        Ok(Stmt::VarDecl { name, ty, init })
    }

    fn parse_type(&self, pair: Pair<Rule>) -> CompileResult<Type> {
        let inner_rule = pair.into_inner().next().unwrap(); // Get the actual type rule (base_type, pointer_type, etc.)
        match inner_rule.as_rule() {
            Rule::base_type => {
                let mut inner_base_type = inner_rule.into_inner();
                let base_name = inner_base_type.next().unwrap().as_str().trim();
                // For now, assume no complex array types directly from parsing this,
                // and defer full array parsing if needed.
                Ok(Type::from_kit(base_name))
            }
            Rule::pointer_type => {
                let inner_ptr_type = inner_rule.into_inner().next().unwrap(); // Get the type being pointed to
                let inner_ty = self.parse_type(inner_ptr_type)?;
                Ok(Type::Ptr(Box::new(inner_ty)))
            }
            // TODO: Handle other type_annotation rules like function_type, tuple_type
            _ => Err(CompilationError::ParseError(format!(
                "Unexpected rule in type_annotation: {:?}",
                inner_rule.as_rule()
            ))),
        }
    }

    fn transpile_with_program(&mut self, prog: Program) {
        let c_code = self.generate_c_code(prog);
        if let Err(e) = std::fs::write(&self.c_output, c_code) {
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

    fn parse_if_stmt(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // if_stmt = { "if" ~ expr ~ block ~ else_part? }
        // else_part = { "else" ~ (block | if_stmt) }
        let mut inner = pair.into_inner();
        let cond = self.parse_expr(inner.next().unwrap())?;
        let then_branch = self.parse_block(inner.next().unwrap())?;

        let mut else_branch = None;
        if let Some(else_pair) = inner.next() {
            assert_eq!(else_pair.as_rule(), Rule::else_part);
            let else_content = else_pair.into_inner().next().unwrap();
            let else_block = match else_content.as_rule() {
                Rule::block => self.parse_block(else_content)?,
                Rule::if_stmt => {
                    let if_stmt = self.parse_if_stmt(else_content)?;
                    Block {
                        stmts: vec![if_stmt],
                    }
                }
                _ => unreachable!(),
            };
            else_branch = Some(else_block);
        }

        Ok(Stmt::If {
            cond,
            then_branch,
            else_branch,
        })
    }

    fn parse_while_stmt(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // while_stmt = { "while" ~ expr ~ block }
        let mut inner = pair.into_inner();
        let cond = self.parse_expr(inner.next().unwrap())?;
        let body = self.parse_block(inner.next().unwrap())?;
        Ok(Stmt::While { cond, body })
    }

    fn parse_for_stmt(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // for_stmt = { "for" ~ identifier ~ "in" ~ expr ~ block }
        let mut inner = pair.into_inner();
        let var = inner.next().unwrap().as_str().to_string();
        let iter = self.parse_expr(inner.next().unwrap())?;
        let body = self.parse_block(inner.next().unwrap())?;
        Ok(Stmt::For { var, iter, body })
    }

    fn parse_expr(&self, pair: Pair<Rule>) -> CompileResult<Expr> {
        match pair.as_rule() {
            Rule::expr => {
                // SAFETY: Grammar guarantees exactly one child for wrapper rules
                let inner = pair.into_inner().next().unwrap();
                self.parse_expr(inner)
            }
            Rule::assign => self.parse_assign_expr(pair),
            Rule::logical_or
            | Rule::logical_and
            | Rule::equality
            | Rule::comparison
            | Rule::additive
            | Rule::multiplicative
            | Rule::bitwise_or
            | Rule::bitwise_xor
            | Rule::bitwise_and
            | Rule::shift => {
                let mut inner = pair.into_inner();
                let mut left = self.parse_expr(inner.next().unwrap())?;

                while let Some(op_pair) = inner.next() {
                    let op = BinaryOperator::from_rule_pair(&op_pair)?;
                    let right = self.parse_expr(inner.next().unwrap())?;
                    left = Expr::BinaryOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                }
                Ok(left)
            }
            Rule::unary => {
                let mut inner_pairs = pair.into_inner();
                // SAFETY: Unary rule always has at least one child
                let first_pair = inner_pairs.next().unwrap();

                match first_pair.as_rule() {
                    Rule::unary_op => {
                        let op_str = first_pair.as_str();
                        let op = UnaryOperator::from_str(op_str)
                            .map_err(|_| parse_error!("invalid unary operation: {}", op_str))?;

                        // SAFETY: Grammar guarantees expression after unary op
                        let expr = self.parse_expr(inner_pairs.next().unwrap())?;
                        Ok(Expr::UnaryOp {
                            op,
                            expr: Box::new(expr),
                        })
                    }
                    Rule::ADDRESS_OF_OP => {
                        // Handle address-of operator explicitly
                        let op = UnaryOperator::AddressOf; // This is always AddressOf
                        let expr = self.parse_expr(inner_pairs.next().unwrap())?;
                        Ok(Expr::UnaryOp {
                            op,
                            expr: Box::new(expr),
                        })
                    }
                    Rule::primary => self.parse_expr(first_pair),
                    _other => Err(parse_error!("Unexpected rule in unary: {:?}", _other)),
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
                                    parse_error!("invalid integer literal '{s}': {:?}", e)
                                })?;
                                Ok(Expr::Literal(Literal::Int(i)))
                            }
                            Rule::float => {
                                let s = num_pair.as_str();
                                let f = s.parse::<f64>().map_err(|e| {
                                    parse_error!("invalid float literal '{s}': {:?}", e)
                                })?;
                                Ok(Expr::Literal(Literal::Float(f)))
                            }
                            _ => Err(parse_error!("Unexpected number type")),
                        }
                    }
                    Rule::boolean => match inner.as_str() {
                        "true" => Ok(Expr::Literal(Literal::Bool(true))),
                        "false" => Ok(Expr::Literal(Literal::Bool(false))),
                        _s => Err(parse_error!("invalid boolean literal: {}", _s)),
                    },
                    Rule::char_literal => todo!("char literal parsing not implemented"),
                    _ => Err(parse_error!(
                        "Unexpected literal type: {:?}",
                        inner.as_rule()
                    )),
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
            Rule::if_expr => {
                let mut inner = pair.into_inner();
                let cond = self.parse_expr(inner.next().unwrap())?;
                let then_branch = self.parse_expr(inner.next().unwrap())?;
                let else_branch = self.parse_expr(inner.next().unwrap())?;
                Ok(Expr::If {
                    cond: Box::new(cond),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                })
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
                    // TODO: Right now, this is a basic implementation and might need to be expanded for more "complex" iterators, like
                    // `for i in 1...10`, etc.
                    let iter_str = self.transpile_expr(iter);
                    let body_code = self.transpile_block(body);
                    code.push_str(&format!(
                        "for (int {} = 0; {} < {}; ++{}) {{\n{}}}",
                        var, var, iter_str, var, body_code
                    ));
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
        }
    }

    fn transpile_type(&self, ty: &Type) -> String {
        ty.to_c_repr().name
    }

    /// Parses an assignment expression (right-associative).
    fn parse_assign_expr(&self, pair: Pair<Rule>) -> CompileResult<Expr> {
        let mut inner = pair.into_inner();
        let left_pair = inner.next().unwrap(); // This is always the LHS (logical_or)

        let left = self.parse_expr(left_pair)?;

        // The grammar rule for `assign` is `assign = { logical_or ~ ASSIGN_OP ~ assign | logical_or }`.
        // This means `inner.next()` after the LHS (`logical_or`) will yield `ASSIGN_OP` if it's an assignment.
        // If there's no assignment, `inner` will be exhausted.

        if let Some(assign_op_pair) = inner.next() {
            // This should be ASSIGN_OP
            let op = AssignmentOperator::from_rule_pair(&assign_op_pair)?;
            let right_assign_expr_pair = inner.next().unwrap(); // This should be the RHS (nested assign)
            let right = self.parse_assign_expr(right_assign_expr_pair)?;
            Ok(Expr::Assign {
                op,
                left: Box::new(left),
                right: Box::new(right),
            })
        } else {
            // No assignment operator, so it's just the expression itself (the logical_or that formed the LHS)
            Ok(left)
        }
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
