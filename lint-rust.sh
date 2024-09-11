#!/bin/bash

cargo fmt --manifest-path $1
cargo clippy --all-targets --all-features --manifest-path $1
