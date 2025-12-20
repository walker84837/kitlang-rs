# Contribution Guidelines

We welcome contributions to the `kitlang-rs` project!

To make sure the contribution process goes smoothly, please follow these guidelines. This guide is intended for who have never contributed to this repository before. Its purpose is to familiarize them with the codebase and explain the contribution process.

## Table of Contents

- [Structure](#structure)
- [What to do](#what-to-do)
  * [Good First Issues](#good-first-issues)
- [How to contribute](#how-to-contribute)
  * [Setting up your development environment](#setting-up-your-development-environment)
  * [Building and running `kitc`](#building-and-running-kitc)
  * [Running tests](#running-tests)
  * [Code style and quality](#code-style-and-quality)
  * [Implementing new features](#implementing-new-features)
  * [Submitting changes (PRs)](#submitting-changes-prs)
- [FAQs](#faqs)
## Structure

I'll go over this structure briefly to get you up to speed. This project has two packages:
* `kitlang`: This is the core compiler library. It handles:
  * Lexing: breaking source code into tokens.
  * Parsing: Building an AST from tokens using `pest` (grammar defined in [`kitlang/src/grammar/kit.pest`](https://github.com/walker84837/kitlang-rs/blob/main/kitlang/src/grammar/kit.pest)).
  * Code Generation: Translating the AST into an intermediate representation and then into executable code.
  * Type Checking, Semantic Analysis, etc. (These are areas that are continually being developed).

* `kitc`: This is the main CLI tool that users interact with. It provides the `kitc` executable, which wraps the functionality of the `kitlang` library to compile `.kit` files. (and maybe initialize Kit projects, like `cargo init`)

## What to do

If you're looking for a good place to start, check out:
- the **"Good First Issues"** section below;
- the **README's Roadmap** section which points at things this project needs help with.

If you have any questions on where you could start, please file an issue, even if you need to ask a question you think is "stupid". I'm happy to provide any further information you need.

### Good First Issues

If you're looking for a simple way to get started, we label issues that are suitable for beginners with ["good first issue"](https://github.com/walker84837/kitlang-rs/labels/good%20first%20issue).

## How to contribute

### Setting up your development environment

To get started, you'll need Rust and Cargo installed. If you don't have them, follow the instructions on the [official Rust website](https://www.rust-lang.org/tools/install).

1. Fork and clone the repository:
   ```bash
   # or you can clone upstream and then change remote
   git clone https://github.com/your-username/kitlang-rs.git
   cd kitlang-rs
   ```

2. Build the project:
    ```bash
    cargo build
    ```
    This will build both the `kitlang` library and the `kitc` command-line tool.

### Building and running `kitc`

To build the `kitc` executable:

```bash
cargo build --bin kitc
```

You can then run it to compile a Kit file:

```bash
cargo run -- kitc compile examples/helloworld.kit
```

### Running tests

Before submitting any changes, please ensure all tests pass:

```bash
cargo test
```

This will also test your changes against actual Kit code samples at `examples/`.

### Code style and quality

We maintain a consistent code style and quality throughout the project:

* Formatting: Always format your code with `cargo fmt`.
  ```bash
  cargo fmt
  ```

* Linting: Check your code for common mistakes and unidiomatic Rust with `cargo clippy`.
  ```bash
  cargo clippy -- -D warnings
  ```
  Please address any warnings reported by Clippy, unless they make the code read worse.
  If you don't know how to resolve a warning, please ask in the PR you're working on.

### Implementing new features

When implementing a new feature for the Kit language, please ensure you:

1. Add minimal example code testing the new feature in the `examples/` directory.
2. Verify that your example compiles correctly using the `kitc` CLI tool.
3. Add unit tests where feasible to make sure the feature works as expected and prevents regressions.

### Submitting changes (PRs)

1. **Fork the repository** and **create a new branch** for your feature or bug fix.
2. **Make your changes**, ensuring they adhere to the code style and testing guidelines.
3. **Write clear, concise commit messages** (preferably following [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/)). If you're unfamiliar with Conventional Commits, don't worry--I won't reject your PR over it.
4. **Push your branch to your fork** and **open a pull request** against the `main` branch of this repo.
5. **Provide a detailed description** of your changes **in the pull request**, and why they make sense in the context of this repository.

## FAQs

1. **When do my changes (or issues) get reviewed?**

   Since I'm the sole maintainer, I don't have all day to review PRs and issues. This means that I may not respond *immediately*, but it will be reviewed within a few hours to a day after.

2. **What types of contributions are accepted?**

   I accept mainly:
   * bug fixes
   * features (i.e. new functionality, grammar implementations, etc.)
   * documentation updates

   Basically any, as long as they make sense in the context of the project.

3. **When is it appropriate to follow up?**

   I check my GitHub inbox fairly often in my free time. If by the day after you haven't heard from me, feel free to ping me.

4. **Do you need to know about compilers to contribute?**

   Not at all. I also appreciate contributions that aren't just about compiler work: it can be writing error messages, or adding example Kit programs. You name it!
