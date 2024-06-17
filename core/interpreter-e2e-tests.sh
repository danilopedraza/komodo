#!/bin/bash

examples_dir="$(pwd)/../examples"

for file in $(find "$examples_dir" -type f -name "*.smtc"); do
    output=$(eval "cargo run $file" 2>&1)

    if [ $? -ne 0 ]; then
        echo "Error: $file failed its execution"
        echo "This is the message from the interpreter:\n"
        echo "$output" >&2
        exit 1
    fi
done

echo "All of the example files were executed correctly!"
