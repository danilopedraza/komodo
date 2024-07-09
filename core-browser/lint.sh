#!/bin/bash

cargo fmt
cargo clippy --all-targets --all-features
