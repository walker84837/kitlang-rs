# kitlang-rs

kitlang-rs is an experimental Rust port of the [Kit compiler](https://github.com/kitlang/kit)[^1], aiming to bring the Kit programming language to the Rust ecosystem.

## Motivation

While the original Kit compiler is written in Haskell, this project was initiated to create a Rust-based alternative. The reasons for not contributing to the original Kit compiler are:

* I'm not very familiar with Haskell, making it hard to understand and contribute to the existing codebase
* The purely functional nature of Haskell, which makes it difficult to translate concepts to a multi-paradigm language like Rust
* A rewrite from scratch offers more flexibility, faster development, and freedom in compiler design choices

Notable links:

* [Original compiler](https://github.com/kitlang/kit)
* [Kit examples](https://web.archive.org/web/20250319015229/https://www.kitlang.org/examples.html)
* Standard library
  - [Section in examples page](https://web.archive.org/web/20250319015229/https://www.kitlang.org/examples.html#standard-library)
  - [Original implementation](https://github.com/kitlang/kit/tree/dev/std)

## Roadmap

The following features are planned for the compiler rewrite:

- Basic compiler infrastructure:
  * [X] (**WIP**) Transpile Kit code to C99 and use the local C compiler for compilation (grammar definition significantly improved)
    - [X] Add linking flags for compatibility with C libraries (detected, but arguments not fully wired yet)
    - [ ] Add custom flags configuration for custom or uncommon compiler toolchain
  * [X] Define comprehensive grammar using `pest.rs`
  * [ ] Implement meaningful error messages instead of panics

- Compiler CLI
  * [ ] Introduce a compilation progress bar for improved user experience
  * [ ] Display elapsed compilation time for performance monitoring

- Package manager:
    - [ ] Cargo-like usage
    - [ ] Kit projects seem to have a [kit.yaml](https://github.com/bendmorris/krit-kit/blob/master/kit.yaml), which looks like a Cargo.toml
    - [ ] Documentation generation (inspired from `cargo doc`)

- Standard library:
  * [ ] Develop a standard library using as much Kit code as possible
  * [ ] Zero-cost abstractions (if possible)

## Licensing

kitlang-rs is dual-licensed under the [Apache License, Version 2.0](LICENSE-APACHE) and the [MIT License](LICENSE-MIT), either at your option.

**DISCLAIMER**: This project is entirely independent of the original [Kit compiler](https://github.com/kitlang/kit) and is not affiliated with the original developers.

[^1]: If you want to be more pedantic, the more correct wording is *transpiler* as it outputs C source code, not machine code.
