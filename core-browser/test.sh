#!/bin/bash

set -e

cargo fmt --check

cargo clippy --all-targets --all-features

cargo test --all-features
