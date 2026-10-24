# Komodo

The Komodo programming language code repository. Read about Komodo in the [website](https://komodo-lang.org)!

## This repo

This is a monorepo with all the code related to Komodo. Here is a quick summary:

- `core`: The Komodo interpreter. Is a Rust project, currently set a up as a program and a library simultaneously.

- `core-browser`: A static website and a Rust project that compiles the interpreter to WASM and creates a simple API to use it in the browser.

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
