#!/bin/bash

set -e

cargo fmt --check

cargo clippy --all-targets --all-features

cargo test --all-features

examples_dir="$(pwd)/../examples"
echo "Running examples..."
for file in $(find "$examples_dir" -type f -name "*.smtc"); do
    output=$(eval "cargo run --quiet $file" 2>&1)

    if [ $? -ne 0 ]; then
        echo "Error: $file failed its execution" >&2
        echo "This is the message from the interpreter:\n" >&2
        echo "$output" >&2
        exit 1
    fi
done

echo "All of the example files were executed correctly!"
