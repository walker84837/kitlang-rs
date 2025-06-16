# kitlang-rs

An experimental Rust port of the [Kit](https://github.com/kitlang/kit) compiler.

## Roadmap

- Basic compiler infrastructure
  - [ ] Transpile Kit to C99 and then use local C compiler to compile the code
  - [ ] Meaningful errors in place of panics
  - [ ] Add linking flags to link with C libraries
- Compiler CLI
  - [ ] Add compilation progress bar
  - [ ] Add elapsed compilation time
- Standard library
  - [ ] Write standard library with as much Kit code as possible

## License

This project is licensed under the [Apache v2](LICENSE-APACHE) license or the [MIT](LICENSE-MIT) license, either one at your choice.
