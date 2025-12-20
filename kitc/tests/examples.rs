use assert_cmd::{Command as AssertCommand, cargo::*};
use predicates::prelude::*;
use std::{path::Path, process::Command};

fn run_example_test(
    example_name: &str,
    stdin: Option<&str>,
) -> Result<(), Box<dyn std::error::Error>> {
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let workspace_root = Path::new(&manifest_dir).parent().unwrap();

    let examples_dir = Path::new("examples");
    let example_file = examples_dir.join(format!("{}.kit", example_name));
    let expected_file = examples_dir.join(format!("{}.kit.expected", example_name));

    let source_path = workspace_root.join(example_file);
    let c_path = source_path.with_extension("c");
    let executable_path = source_path.with_extension("");

    // Compile the example
    let mut cmd = AssertCommand::from_std(Command::new(cargo_bin!("kitc")));

    // Run from workspace root
    cmd.current_dir(workspace_root);
    cmd.arg("compile").arg(&source_path);
    cmd.assert().success();

    // Run the compiled executable
    let mut compiled_cmd = AssertCommand::new(&executable_path);
    if let Some(stdin_data) = stdin {
        compiled_cmd.write_stdin(stdin_data);
    }

    let expected_output = std::fs::read_to_string(workspace_root.join(expected_file))?;

    // Assert the output
    compiled_cmd
        .assert()
        .stdout(predicate::eq(expected_output.as_str()))
        .success();

    // Clean up the executable and .c file
    std::fs::remove_file(&executable_path)?;
    std::fs::remove_file(&c_path)?;

    Ok(())
}

#[test]
fn test_helloworld() -> Result<(), Box<dyn std::error::Error>> {
    run_example_test("helloworld", None)
}

#[test]
fn test_bitwise_not() -> Result<(), Box<dyn std::error::Error>> {
    run_example_test("bitwise_not", None)
}

#[test]
fn test_input() -> Result<(), Box<dyn std::error::Error>> {
    run_example_test("input", Some("42\n"))
}
