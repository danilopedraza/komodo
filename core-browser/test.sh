#!/bin/bash

set -e

cargo fmt --check

cargo clippy --all-targets --all-features

cargo test --all-features -- --test-threads=1

wasm-pack build --target web
