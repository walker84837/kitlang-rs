use crate::codegen::types::*;
use crate::{KitParser, Rule};
use pest::Parser;
use pest::iterators::Pair;
use std::path::{Path, PathBuf};
use std::process::Command;

pub enum Optimizations {
    Simple = 1,
    Medium = 2,
    Aggressive = 3,
}

pub struct Compiler {
    pub files: Vec<PathBuf>,
    pub output: PathBuf,
    pub opt_level: Option<Optimizations>,
    includes: Vec<Include>,
}

impl Compiler {
    pub fn new(
        files: Vec<PathBuf>,
        output: impl AsRef<Path>,
        opt_level: Option<Optimizations>,
    ) -> Self {
        Self {
            files,
            output: output.as_ref().to_path_buf(),
            includes: Vec::new(),
            opt_level,
        }
    }

    fn parse(&mut self) -> Vec<Function> {
        let mut includes = Vec::new();
        let mut functions = Vec::new();
        for file in &self.files {
            let input = std::fs::read_to_string(file).expect("Failed to read file");
            let mut pairs = KitParser::parse(Rule::program, &input)
                .expect("Parse failed")
                .into_iter();
            while let Some(pair) = pairs.next() {
                match pair.as_rule() {
                    Rule::include => includes.push(self.parse_include(pair)),
                    Rule::function => functions.push(self.parse_function(pair)),
                    _ => {}
                }
            }
        }
        // store includes/imports somewhere or return a richer AST
        // for now, stash in a field or return a Program struct
        // e.g. Program { includes, imports, functions }
        // but to keep minimal, we might store them on the Compiler
        self.includes = includes;
        functions
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

        // Function name
        let name = inner.next().unwrap().as_str().to_string();

        // Parse parameters if present
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

        // Parse return type if present
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

        Function {
            name,
            params,
            return_type,
            body,
        }
    }

    fn parse_params(&self, pair: Pair<Rule>) -> Vec<Param> {
        // param_list = { param ~ ("," ~ param )* }
        pair.into_inner()
            .filter(|p| p.as_rule() == Rule::param)
            .map(|p| {
                let mut inner = p.into_inner();
                let name = inner.next().unwrap().as_str().to_string();
                let ty = inner.next().unwrap().as_str().to_string();
                Param {
                    name,
                    ty: Type::Named(ty),
                }
            })
            .collect()
    }

    fn parse_block(&self, pair: Pair<Rule>) -> Block {
        // block = { "{" ~ stmt* ~ "}" }
        let stmts = pair
            .into_inner()
            .filter_map(|p| match p.as_rule() {
                Rule::var_decl => Some(self.parse_var_decl(p)),
                Rule::expr_stmt => Some(self.parse_expr_stmt(p)),
                Rule::return_stmt => Some(self.parse_return(p)),
                _ => None,
            })
            .collect();
        Block { stmts }
    }

    fn parse_var_decl(&self, pair: Pair<Rule>) -> Stmt {
        // var_decl = { ("var"|"const") ~ ident ~ (":" ~ type_annotation)? ~ ("=" ~ expr)? ~ ";" }
        let mut inner = pair.into_inner();
        let _kw = inner.next().unwrap(); // var
        let name = inner.next().unwrap().as_str().to_string();
        let next = inner.next().unwrap();
        // maybe has explicit type
        let (ty, after_ty) = if next.as_rule() == Rule::type_annotation {
            (Some(self.parse_type(next)), inner.next().unwrap())
        } else {
            (None, next)
        };

        // maybe initializer
        let init = if after_ty.as_rule() == Rule::expr {
            Some(self.parse_expr(after_ty))
        } else {
            None
        };

        Stmt::VarDecl { name, ty, init }
    }

    fn parse_type(&self, pair: Pair<Rule>) -> Type {
        // pair.as_str() gives e.g. "int" or "Foo[Bar]"
        let mut inner = pair.into_inner();
        let base = inner.next().unwrap().as_str().to_string();
        let mut ty = Type::Named(base);
        // if there are array or pointer annotations, handle here…
        for sub in inner {
            // you can extend to multi-dimensional or pointer types
            let inner_ty = self.parse_type(sub);
            ty = Type::Ptr(Box::new(inner_ty));
        }
        ty
    }

    fn optimize(&self) {
        // TODO: implement real optimizations
        if let Some(Optimizations::Simple) = self.opt_level {
            eprintln!("Simple optimization: (none implemented)");
        }
    }

    fn transpile(&mut self) {
        let functions = self.parse();
        let c_code = self.generate_c_code(functions);
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
        match pair.as_rule() {
            Rule::identifier => Expr::Identifier(pair.as_str().to_string()),
            Rule::literal => {
                let s = pair.as_str();
                match s {
                    "true" => Expr::Literal(Literal::Bool(true)),
                    "false" => Expr::Literal(Literal::Bool(false)),
                    "null" => Expr::Literal(Literal::Null),
                    _ => {
                        let i = s
                            .parse::<i64>()
                            .expect("number literal should parse as i64");
                        Expr::Literal(Literal::Int(i))
                    }
                }
            }
            Rule::float => Expr::Literal(Literal::Float(pair.as_str().parse().unwrap())),
            // TODO: string literal
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
            _ => panic!("Unexpected expr rule: {:?}", pair.as_rule()),
        }
    }

    fn generate_c_code(&self, functions: Vec<Function>) -> String {
        let mut c_code = String::new();
        for inc in &self.includes {
            let line = if inc.path.ends_with(".h") {
                if inc.path.starts_with('<') {
                    format!("#include {}\n", inc.path)
                } else {
                    format!("#include \"{}\"\n", inc.path)
                }
            } else {
                // fallback
                format!("#include \"{}\"\n", inc.path)
            };
            c_code.push_str(&line);
        }
        c_code.push_str("\n");

        for func in functions {
            c_code.push_str(&self.transpile_function(&func));
            c_code.push_str("\n\n");
        }

        c_code
    }

    fn transpile_function(&self, func: &Function) -> String {
        let return_type = match &func.return_type {
            Some(Type::Int) => "int",
            Some(Type::Float) => "float",
            Some(Type::CString) => "char*",
            Some(Type::Ptr(inner)) => return format!("{}*", self.transpile_type(inner)),
            Some(Type::Named(name)) => name,
            None => "void",
        };

        let params = func
            .params
            .iter()
            .map(|p| format!("{} {}", self.transpile_type(&p.ty), p.name))
            .collect::<Vec<_>>()
            .join(", ");

        let body = self.transpile_block(&func.body);

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
        }
    }

    pub fn compile(&mut self) {
        self.parse();
        self.optimize();
        self.transpile();

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

        let status = Command::new("gcc")
            .arg(&out_c)
            .arg("-o")
            .arg(&exe_name)
            .status()
            .expect("Failed to run gcc");

        if !status.success() {
            panic!("Compilation failed");
        }
    }
}
