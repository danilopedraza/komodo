#!/bin/bash

set -e

cargo fmt --check

cargo clippy --all-targets --all-features

cargo test --all-features

set +e
examples_dir=$1
echo "Building the interpreter in release mode..."
cargo build --release

echo "Running examples..."
someone_failed=false
for file in $(find "$examples_dir" -type f -name "*.komodo"); do
    output=$(eval "cargo run --release --quiet $file" 2>&1)

    if [ $? -ne 0 ]; then
        echo "❌ Error: $file failed its execution" >&2
        echo "This is the message from the interpreter:" >&2
        echo "$output" >&2
        someone_failed=true
    fi
done

if [ "$someone_failed" = true ] ; then
    exit 1
fi

echo "✅ All the example files were executed successfully!"
echo "All the tests passed!"
