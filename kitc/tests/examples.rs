use assert_cmd::{Command as AssertCommand, cargo::*};
use predicates::prelude::*;
use std::{path::Path, process::Command, sync::OnceLock};

static LOGGER_INIT: OnceLock<()> = OnceLock::new();

fn setup_logging() {
    LOGGER_INIT.get_or_init(|| {
        env_logger::builder().is_test(true).init();
    });
}

fn run_example_test(
    example_name: &str,
    stdin: Option<&str>,
) -> Result<(), Box<dyn std::error::Error>> {
    setup_logging();
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let workspace_root = Path::new(&manifest_dir).parent().unwrap();

    let examples_dir = Path::new("examples");
    let example_file = examples_dir.join(format!("{}.kit", example_name));
    let expected_file = examples_dir.join(format!("{}.kit.expected", example_name));

    log::info!(
        "Running example {} in {} (path: {}). Expected file is at {}",
        example_name,
        workspace_root.display(),
        example_file.display(),
        expected_file.display()
    );

    let source_path = workspace_root.join(example_file);
    let c_path = source_path.with_extension("c");
    let executable_path = source_path.with_extension("");

    // Compile the example
    let kitc = cargo_bin!("kitc");
    log::info!("kitc path: {}", kitc.display());
    let mut cmd = AssertCommand::from_std(Command::new(kitc));

    // Run from workspace root
    cmd.current_dir(workspace_root);
    cmd.arg("compile").arg(&source_path);
    cmd.assert().success();

    // Run the compiled executable
    let mut compiled_cmd = AssertCommand::new(&executable_path);
    if let Some(stdin_data) = stdin {
        compiled_cmd.write_stdin(stdin_data);
    }

    let asjdlksaj = workspace_root.join(expected_file);
    log::info!("Expected output: {}", asjdlksaj.display());
    let expected_output = std::fs::read_to_string(asjdlksaj)?;

    // Assert the output
    compiled_cmd
        .assert()
        .stdout(predicate::eq(expected_output.as_str()))
        .success();

    // TODO: executable files are actually generated in the CWD, not in the examples folder.
    // This explains why the executable is not actually removed. But I don't get why these tests
    // passed on Linux and Mac if std::fs::remove_file is supposed to also fail when the file
    // doesn't exist.
    if let Err(err) = std::fs::remove_file(&executable_path) {
        log::error!("Failed to remove executable: {err}");
    }
    if let Err(err) = std::fs::remove_file(&c_path) {
        log::error!("Failed to remove C source file: {err}");
    }

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
