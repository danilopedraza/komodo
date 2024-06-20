#!/bin/bash

if [ $1 = "prod" ]; then
    set -e
fi

cargo fmt --check
if [ $? -ne 0 ]; then
    while true; do
        read -r -p "Do you want to run 'cargo fmt'? [Y/n] " response
        case "$response" in
            Y|y|"" ) cargo fmt ; break; ;;
            N|n    ) exit 1; ;;
            .*     ) continue; ;;
        esac
    done
fi

cargo clippy --all-targets --all-features

cargo test

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
