# kitlang-rs

> [!IMPORTANT]
> Status: Early-stage, actively developed
>
> See [ROADMAP.md](ROADMAP.md) for planned work and contribution ideas.

kitlang-rs is an early-stage, experimental Rust reimplementation of the [Kit compiler](https://github.com/kitlang/kit)[^1], aiming to make the Kit programming language easier to contribute to and extend.

We have a website at <https://www.kitlang.dev>[^2].

## Motivation

While the original Kit compiler is written in Haskell, this project was initiated to create a Rust-based alternative. The reasons for not contributing to the original Kit compiler are:

* I'm not very familiar with Haskell, making it hard to understand and contribute to the existing codebase
* The purely functional nature of Haskell, which makes it difficult to translate concepts to a multi-paradigm language like Rust
* A rewrite from scratch offers more flexibility, potentially faster iteration, and freedom in compiler design choices

## Quick Start

Here's a simple "Hello World!" example in Kit:

```c
include "stdio.h";

function main() {
    var s: CString = "Hello from Kit!";
    printf("%s\n", s);
}
```

To compile and run this, save it as `hello.kit` and then execute:

```bash
cargo run --bin kitc compile hello.kit
./hello
```

Expected output:

```
Hello from Kit!
```

## Contributing

We warmly welcome contributions!

We have contribution guidelines in [CONTRIBUTING.md](CONTRIBUTING.md) and a detailed roadmap at [ROADMAP.md](ROADMAP.md).

**Notable links**:

* [Original compiler](https://github.com/kitlang/kit)
* [Kit examples](https://kitlang.dev/examples/)
* Standard library
  - [Section in examples page](https://kitlang.dev/examples/#standard-library)
  - [Original implementation](https://github.com/kitlang/kit/tree/dev/std)

## Licensing

kitlang-rs is dual-licensed under the [Apache License, Version 2.0](LICENSE-APACHE) and the [MIT License](LICENSE-MIT), either at your option.

**DISCLAIMER**: This project is entirely independent of the original [Kit compiler](https://github.com/kitlang/kit) and is not affiliated with the original developers.

[^1]: If you want to be more pedantic, the more correct wording is *transpiler* as it outputs C source code, not machine code.
[^2]: The site is a clone of <https://github.com/kitlang/kitlang.github.io>, as the original site is down. See [issue #157](https://github.com/kitlang/kit/issues/157) upstream.
