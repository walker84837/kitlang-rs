use crate::KitParser;
use crate::Rule;
use crate::codegen::types::*;
use pest::Parser;
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
    verbose_messages: bool,
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
            verbose_messages: false,
            opt_level,
        }
    }

    fn parse(&self) -> Vec<Function> {
        let mut functions = Vec::new();
        for file in &self.files {
            let input = std::fs::read_to_string(file).expect("Failed to read file");
            let pairs = KitParser::parse(Rule::program, &input).expect("Parse failed");
            // TODO: process pairs into functions
            println!("{:#?}", pairs);
        }
        functions
    }

    fn optimize(&self) {
        todo!()
    }

    fn transpile(&self) {
        let functions = self.parse();
        let c_code = self.generate_c_code(functions);
        std::fs::write(&self.output, c_code).expect("Failed to write output");
    }

    fn generate_c_code(&self, functions: Vec<Function>) -> String {
        let mut c_code = String::from("#include <stdio.h>\n\n");
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
                    let init_str = self.transpile_expr(init);
                    code.push_str(&format!("{} {} = {};\n", ty_str, name, init_str));
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

    pub fn compile(&self) {
        self.parse();
        self.optimize();
        self.transpile();

        // TODO: compile generated C code with system compiler
        let output_name = self.output.file_stem().unwrap().to_str().unwrap();
        let status = Command::new("gcc")
            .arg("-o")
            .arg(output_name)
            .arg(&self.output)
            .status()
            .expect("Failed to run gcc");

        if !status.success() {
            panic!("Compilation failed");
        }
    }
}
