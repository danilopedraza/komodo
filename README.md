# Komodo

The Komodo programming language code repository. Read about Komodo in the [website](https://komodo-lang.org)!

## This repo

This is a monorepo with all the code related to Komodo. Here is a quick summary:

- `core`: The Komodo interpreter. Is a Rust project, currently set a up as a program and a library simultaneously.

  The compiler pipeline is source → CST → AST → typed AST → stack IR → interpreter. The [IR](core/src/ir.rs) uses an operand stack, structured control blocks, and abstract runtime operations. It retains inferred types and source positions. Closures and named functions store IR bodies. This provides a separate lowering boundary for a future Wasm backend.

- `core-browser`: A static website and a Rust project that compiles the interpreter to Wasm and creates a simple API to use it in the browser.

- `vsc-extension`: The VSC extension for Komodo. It's a Node project.

- `examples`: A few simple programs written in Komodo. They are executed in CI to test the interpreter.

- `book`: The Komodo book. It is an MdBook project, so it's written in Markdown.

- `std`: Some Komodo files with utility functions.

- `website`: The Komodo landing page.

- `report`: A document, in spanish, explaining the language and the interpreter. I am writing it with Typst. You can read it [here](https://komodo-lang.org/report.pdf).

- `installer`: a directory with install scripts for the interpreter and the standard library.

## Hacking

The repository has a Nix flake containing the development tools and build outputs. Rust dependencies and builds are handled by [Crane](https://github.com/ipetkov/crane).

You can create a dev shell with these programs typing

```
nix develop
```

Run that in the repository root. The existing Makefiles are the convenient command interface inside the development shell. You can also build the interpreter directly with `nix build`, or run all flake checks with `nix flake check`.

CI uses the locked flake for builds and checks, with the Nix store cached between
GitHub Actions runs. Run the same checks locally:

```sh
nix flake check -L
nix run .#cargo-test
nix run .#examples
nix run .#clippy
nix run .#formatting
nix run .#browser-test
nix run .#browser-clippy
nix run .#browser-formatting
nix run .#smoke
```

Individual checks are also available as `nix build .#checks.<system>.<name>`.
Successful checks are cached; `nix run` reuses their results until their inputs
change. Build artifacts with `nix build .#komodo`, `.#komodo-release`,
`.#playground`, `.#website`, `.#book`, `.#report`, or `.#extension`.
`komodo-release` is statically linked on Linux so the downloaded release binary
works without Nix. The website output contains the playground, book, report, and
installer; the extension output contains `extension.vsix`.

`nix run .#bench -- <previous-commit>` benchmarks both revisions with the current
locked Nix build recipe, without changing your checkout. It requires
`BENCHER_API_TOKEN` for uploading results. Benchmark timing depends on the host;
the binaries and benchmark tooling are Nix builds. `nix run .#publish-extension`
publishes the built VSIX using `VSCE_PAT` and `OVSX_PAT`.

When updating browser dependencies, keep `core-browser/Cargo.lock`'s
`wasm-bindgen` version aligned with the flake's `wasm-bindgen-cli` version.
