# Roadmap

This document outlines the planned development of the Rust-based Kit compiler.
The project is currently in the **bootstrapping phase**, focusing on a minimal but correct
compiler pipeline.

The roadmap is organized into *phases*, not strict deadlines.

## Phase 0: Core Compiler Infrastructure (current)

Goal: compile a small but valid subset of Kit to C99 and produce a working binary.

### Parsing & Representation

- [ ] Grammar definition closely aligned with the original Haskell AST
- [ ] Stable AST representation in Rust

### Code Generation (C backend)

Goal: lower a well-defined subset of Kit ("Kit Core") to portable C99.

#### Supported language features (Kit Core)

- [X] Top-level functions
- [X] Local variables (`var`)
- [X] Primitive types:
  - [X] `Int`
  - [X] `Bool`
  - [X] `CString`
- [X] `if` expressions / statements
- [ ] `while` and `for` loops
  - [X] Basic implementation
  - [x] `for i in X...Y`
  - [ ] [`kit.iterator`](https://kitlang.dev/examples/#kititerator)
- [X] Function calls
- [X] `return`
- [X] `include` statements for C headers
- [X] Interoperability with C functions (e.g. `printf`)

#### Explicitly unsupported (for now)

- [ ] Generics
- [ ] Term rewriting
- [ ] `defer`
- [ ] Pattern matching
- [ ] Implicits

#### Backend behavior

- [ ] Generate valid, readable C99 source code
- [X] Invoke the system C compiler to produce a binary
  - [ ] Configurable compiler flags and toolchain
- [ ] Remove intermediate C files after successful compilation

### Error Handling

- [x] Replace panics with meaningful compiler errors
- [ ] Add error location information (line and column)
- [ ] Show a source code snippet for errors

### Testing

- [x] Unit testing using examples in `examples/`

## Phase 1: Compiler CLI & Developer Experience

Goal: improve usability and feedback during compilation.

- [x] Display elapsed compilation time
- [ ] Show compilation progress (progress bar or structured stages)

## Phase 2: Standard Library

Goal: provide a minimal but practical standard library.

- [ ] Use the original Kit stdlib as a starting point, with license and authorship disclaimers
- [ ] Provide C interoperability where needed
- [ ] Aim for zero-cost abstractions where possible

## Phase 3: Package Manager & Project Workflow

Goal: make Kit projects ergonomic to build and manage.

- [ ] Introduce a Cargo-like workflow for Kit projects
- [ ] Support `kit.yaml` project manifests
- [ ] Documentation generation (inspired by `cargo doc`)

More information at <https://docs.kitlang.dev/package-management/>

## Phase 4: Documentation & Project Infrastructure

Goal: support users and contributors beyond the compiler itself.

- [x] Provide a landing page with examples and documentation (kitlang.dev)
- [ ] Compiler-specific documentation (architecture, internals, design decisions; docs.kitlang.dev)
- [ ] Blog / news section for project updates (optional, future)
- [ ] Set a Kit Style Guide based on stdlib code and Kit repos (optional, future). More information on this at <https://docs.kitlang.dev/style-guide/>

## Completed Work

These features are already implemented:

- Add linking flags for compatibility with C libraries
- Replace compiler panics with structured error messages
- Unit testing via example-based tests
- Display elapsed compilation time during compilation
