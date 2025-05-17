use crate::parser::{KitParser, Rule};
use pest::Parser;
use std::path::{Path, PathBuf};
use std::process::Command;

enum Optimizations {
    Simple = 1,
    Medium = 2,
    Aggressive = 3,
}

struct Compiler {
    files: Vec<PathBuf>,
    output: PathBuf,
    verbose_messages: bool,
    opt_level: Option<Optimizations>,
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

    fn parse(&self) {
        for file in &self.files {
            let input = std::fs::read_to_string(file).expect("Failed to read file");
            let pairs = KitParser::parse(Rule::program, &input).expect("Parse failed");
            println!("pairs: {pairs:#?}");
            todo!()
        }
    }

    fn optimize(&self) {
        todo!()
    }

    fn transpile(&self) {
        let c_code = self.generate_c_code();
        std::fs::write(&self.output, c_code).expect("Failed to write output");
    }

    fn generate_c_code(&self) -> String {
        // TODO: write C transpiler to convert AST -> C
        String::from(
            "#include <stdio.h>\n\nint main() {\n    printf(\"Hello, World!\\n\");\n    return 0;\n}\n",
        )
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
