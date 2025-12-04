use crate::codegen::{
    compiler::{self, CompilerMeta, CompilerOptions},
    types::*,
};
use crate::{KitParser, Rule};
use log::debug;

use pest::{Parser, iterators::Pair};

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Command;

pub enum Optimizations {
    // Should be handled by the compiler itself
    Simple = 1,
    Medium = 2,
    Aggressive = 3,
}

pub struct Compiler {
    pub files: Vec<PathBuf>,
    pub output: PathBuf,
    includes: Vec<Include>,
}

impl Compiler {
    pub fn new(files: Vec<PathBuf>, output: impl AsRef<Path>) -> Self {
        Self {
            files,
            output: output.as_ref().to_path_buf(),
            includes: Vec::new(),
        }
    }

    fn parse(&mut self) -> Program {
        let mut includes = Vec::new();
        let mut functions = Vec::new();

        for file in &self.files {
            let input = std::fs::read_to_string(file).expect("Failed to read file");
            let pairs = KitParser::parse(Rule::program, &input).expect("Parse failed");

            for pair in pairs {
                match pair.as_rule() {
                    Rule::include => includes.push(self.parse_include(pair)),
                    Rule::function => functions.push(self.parse_function(pair)),
                    _ => {}
                }
            }
        }

        self.includes = includes.clone();

        Program {
            includes,
            imports: HashSet::new(),
            functions,
        }
    }

    fn parse_include(&self, pair: Pair<Rule>) -> Include {
        // include = { "include" ~ string ~ ";" }
        let mut inner = pair.into_inner();
        let lit = inner.next().unwrap().as_str();
        // lit includes the quotes, so strip them:
        let path = lit[1..lit.len() - 1].to_string();
        Include { path }
    }

    fn parse_function(&self, pair: Pair<Rule>) -> Function {
        let mut inner = pair.into_inner();

        let name = inner.next().unwrap().as_str().to_string();
        debug!("parse_function: Function name: {}", name);

        let params = if let Some(node) = inner.peek() {
            if node.as_rule() == Rule::params {
                let params_node = inner.next().unwrap();
                self.parse_params(params_node)
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        debug!("params: {:#?}", params);
        let return_type = if let Some(node) = inner.peek() {
            if node.as_rule() == Rule::type_annotation {
                let type_node = inner.next().unwrap();
                Some(self.parse_type(type_node))
            } else {
                None
            }
        } else {
            None
        };

        // Parse function body
        let body_node = inner.next().expect("function body block");
        let body = self.parse_block(body_node);
        debug!("body: {:#?}", body);

        Function {
            name,
            params,
            return_type,
            body,
        }
    }

    fn parse_params(&self, pair: Pair<Rule>) -> Vec<Param> {
        // param_list = { param ~ ("," ~ param )* }
        let params = pair
            .into_inner()
            .filter(|p| p.as_rule() == Rule::param)
            .map(|p| {
                let mut inner = p.into_inner();
                let name = inner.next().unwrap().as_str().to_string();
                let type_node = inner.next().unwrap();
                let ty = self.parse_type(type_node);
                Param { name, ty }
            })
            .collect();
        debug!("params: {:#?}", params);
        params
    }

    fn parse_block(&self, pair: Pair<Rule>) -> Block {
        // block = { "{" ~ (statement)* ~ "}" }
        let stmts = pair
            .into_inner()
            // grammar gives us a wrapper Rule::statement
            .filter(|p| p.as_rule() == Rule::statement)
            .map(|stmt_pair| {
                // unwrap the single child inside statement
                let inner = stmt_pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::var_decl => self.parse_var_decl(inner),
                    Rule::expr_stmt => self.parse_expr_stmt(inner),
                    Rule::return_stmt => self.parse_return(inner),
                    other => unreachable!("unexpected statement: {:?}", other),
                }
            })
            .collect();
        Block { stmts }
    }

    fn parse_var_decl(&self, pair: Pair<Rule>) -> Stmt {
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
                    ty = Some(self.parse_type(child));
                }
                Rule::expr => {
                    init = Some(self.parse_expr(child));
                }
                _ => { /* skip punctuation and the 'var'/'const' keyword */ }
            }
        }

        // name must be present
        let name = name.expect("var_decl missing identifier");
        Stmt::VarDecl { name, ty, init }
    }

    fn parse_type(&self, pair: Pair<Rule>) -> Type {
        let mut inner = pair.into_inner();
        let base = inner.next().unwrap().as_str();
        // turn known names into their enum cases
        let mut ty = match base {
            "Int" => Type::Int,
            "Float" => Type::Float,
            "Char" => Type::Char,
            "Size" => Type::Size,
            "CString" => Type::CString,
            other => Type::Named(other.to_string()),
        };

        // if there are array or pointer annotations, handle here…
        for sub in inner {
            // you can extend to multi-dimensional or pointer types
            let inner_ty = self.parse_type(sub);
            ty = Type::Ptr(Box::new(inner_ty));
        }
        ty
    }

    fn transpile_with_program(&mut self, prog: Program) {
        let c_code = self.generate_c_code(prog);
        std::fs::write(&self.output, c_code).expect("Failed to write output");
    }

    fn parse_expr_stmt(&self, pair: Pair<Rule>) -> Stmt {
        // expr_stmt = { expr ~ ";" }
        let expr_pair = pair.into_inner().next().unwrap();
        Stmt::Expr(self.parse_expr(expr_pair))
    }

    fn parse_return(&self, pair: Pair<Rule>) -> Stmt {
        // return_stmt = { "return" ~ expr? ~ ";" }
        let mut inner = pair.into_inner();
        let expr = inner.next().map(|p| self.parse_expr(p));
        Stmt::Return(expr)
    }

    fn parse_expr(&self, pair: Pair<Rule>) -> Expr {
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
            | Rule::unary => {
                let inner = pair.into_inner().next().unwrap();
                return self.parse_expr(inner);
            }
            _ => {}
        }

        // now handle the real terminals
        match pair.as_rule() {
            Rule::identifier => Expr::Identifier(pair.as_str().to_string()),
            Rule::literal => {
                let s = pair.as_str();
                match s {
                    "true" => Expr::Literal(Literal::Bool(true)),
                    "false" => Expr::Literal(Literal::Bool(false)),
                    "null" => Expr::Literal(Literal::Null),
                    _ => {
                        // integer literal
                        let i = s.parse::<i64>().expect("invalid int literal");
                        Expr::Literal(Literal::Int(i))
                    }
                }
            }
            Rule::float => {
                let f = pair.as_str().parse::<f64>().expect("invalid float literal");
                Expr::Literal(Literal::Float(f))
            }
            Rule::string => {
                let full = pair.as_str();
                let inner = &full[1..full.len() - 1];
                Expr::Literal(Literal::String(inner.to_string()))
            }
            Rule::function_call => {
                let mut inner = pair.into_inner();
                let callee = inner.next().unwrap().as_str().to_string();
                let args = inner
                    .filter(|p| p.as_rule() == Rule::expr)
                    .map(|p| self.parse_expr(p))
                    .collect();
                Expr::Call { callee, args }
            }
            Rule::primary => {
                let inner = pair.into_inner().next().unwrap();
                self.parse_expr(inner)
            }
            other => panic!("Unexpected expr rule: {:?}", other),
        }
    }

    fn generate_c_code(&self, prog: Program) -> String {
        let mut seen = HashSet::new();
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

        // scan every function signature & body for types to gather their headers/typedefs
        for func in &prog.functions {
            // return type
            if let Some(r) = &func.return_type
                && let Some(hdr) = type_to_c(r).required_header.clone()
            {
                seen.insert(hdr);
            }
            // params
            for p in &func.params {
                if let Some(hdr) = type_to_c(&p.ty).required_header.clone() {
                    seen.insert(hdr);
                }
            }
            // body: any local decls with explicit types
            for stmt in &func.body.stmts {
                if let Stmt::VarDecl { ty: Some(t), .. } = stmt
                    && let Some(hdr) = type_to_c(t).required_header.clone()
                {
                    seen.insert(hdr);
                }
            }
        }

        // emit each unique header or typedef
        for hdr in seen {
            if hdr.starts_with("typedef") {
                // raw typedef
                out.push_str(&hdr);
            } else {
                // assume it's "<...>"
                out.push_str(&format!("#include {}\n", hdr));
            }
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

        format!("{} {}({}) {{\n{}\n}}", return_type, func.name, params, body)
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
            Expr::Literal(lit) => match lit {
                Literal::Int(i) => i.to_string(),
                Literal::Float(f) => f.to_string(),
                Literal::String(s) => format!("\"{}\"", s),
                Literal::Bool(b) => b.to_string(),
                Literal::Null => "NULL".to_string(),
            },
            Expr::Call { callee, args } => {
                let args_str = args
                    .iter()
                    .map(|a| self.transpile_expr(a))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", callee, args_str)
            }
        }
    }

    fn transpile_type(&self, ty: &Type) -> String {
        match ty {
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::CString => "char*".to_string(),
            Type::Ptr(inner) => format!("{}*", self.transpile_type(inner)),
            Type::Named(name) => name.clone(),
            _ => todo!("Unsupported type: {:#?}", ty),
        }
    }

    pub fn compile(&mut self) {
        let prog = self.parse();
        self.transpile_with_program(prog);

        let out_c = if self.output.extension().unwrap_or_default() == "c" {
            self.output.clone()
        } else {
            // ensure .c extension
            self.output.with_extension("c")
        };

        let exe_name = self
            .output
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        let detected = compiler::get_system_compiler().unwrap();
        let opts = CompilerOptions::new(CompilerMeta(detected.0.clone(), detected.1.clone()))
            .link_libs(&["m"])
            .lib_paths(&["/usr/local/lib"])
            .targets(&[out_c.clone().into_os_string().into_string().unwrap()])
            .build();

        let mut cmd = Command::new(detected.1.clone());
        cmd.arg(out_c.clone());

        if detected.0.is_unix_like() {
            cmd.arg("-o").arg(&exe_name);
        } else {
            todo!("Unsupported compiler: {:#?}", detected.0);
        }

        cmd.args(&opts.link_opts);

        let output = cmd.output().expect("failed to execute compiler");
        let status = output.status;

        if !status.success() {
            panic!("Compilation failed");
        }
    }
}
