# Repository Guidelines

## Project Structure & Module Organization

- `core/`: Rust interpreter and CLI. Compiler stages live in `core/src/`, integration tests in `core/tests/`, and unit tests alongside implementation code. The pipeline is source → CST → AST → typed AST → stack IR → interpreter.
- `core-browser/`: Rust/Wasm wrapper and HTML/CSS/JavaScript playground.
- `std/` and `examples/`: Komodo standard-library utilities and executable `.komodo` examples.
- `bench/`: Benchmark workloads and Rust tooling.
- `vsc-extension/`: VS Code language configuration and syntax grammar.
- `book/`, `website/`, `report/`, and `installer/`: mdBook documentation, landing page, Typst report, and installation scripts.
- `flake.nix`, `nix/`, and `.github/workflows/`: Reproducible builds and CI automation.

## Build, Test, and Development Commands

Run commands from the repository root unless specified:

- `nix develop`: Enter the shell with Rust, Wasm, and documentation tools.
- `nix build .#komodo`: Build the interpreter; `nix build .#playground` builds the browser playground.
- `make repl`: Start the interpreter with `KOMODO_STD` configured.
- `make test-core`: Run Rust tests and executable examples.
- `make test-core-browser`: Check formatting, lint, test serially, and build Wasm.
- `nix flake check -L`: Run the checks used by CI on Linux and macOS.
- `make serve-book`: Serve documentation locally.

## Coding Style & Naming Conventions

Use Rust 2021 conventions: four-space indentation, `snake_case` functions/modules, and `PascalCase` types. Run `cargo fmt --manifest-path core/Cargo.toml` and `make lint-core`; use the corresponding browser manifest and `make lint-core-browser` for browser changes. Match surrounding style in web assets and configuration files; preserve tabs in Makefile recipes.

## Testing Guidelines

Tests that execute whole Komodo programs belong in `examples/` as end-to-end `.komodo` tests with assertions, run by `make test-core`. Do not embed whole-program execution tests in Rust unit tests or `core/tests/`; this is a permanent testing preference. Keep Rust tests focused on individual functions, compiler stages, and internal invariants, using `#[test]` with descriptive `snake_case` names near the affected code or in `core/tests/`.

Browser tests must run with `--test-threads=1` because they share global I/O buffers. No numeric coverage threshold is configured.

## Commit & Pull Request Guidelines

History favors short, imperative, often lowercase subjects such as `make clippy happy`; no mandatory prefix is evident. Keep commits focused. PRs should explain the behavior change, link relevant issues, and report validation commands/results. Include screenshots for visible playground or website changes.

## Configuration Tips

Keep the browser lockfile's `wasm-bindgen` version aligned with the flake's `wasm-bindgen-cli`. Keep publishing and benchmark API tokens out of commits.
