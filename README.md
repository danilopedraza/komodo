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

## Hacking

I use a very simple Nix setup to develop and deploy everything. The `shell.nix` file contains all the requirements (I think) to build everything.

You can create a dev shell with these programs typing

```
nix-shell
```

In the repo root directory. There are some convenient commands in the `Makefile`, as well as some commands that I use for deployment.
