#!/bin/bash

# Check if golangci-lint is installed
if ! command -v golangci-lint &> /dev/null; then
    echo "golangci-lint could not be found. Please install it first."
    exit 1
fi

# Find directories containing go.mod files
find . -name "go.mod" | while read -r go_mod; do
    # Get the directory of the go.mod file
    dir=$(dirname "$go_mod")

    # Check for modified Go files in the directory
    if git diff --name-only HEAD "$dir" | grep -q '\.go$'; then
        echo "Running golangci-lint in $dir"
        (cd "$dir" && golangci-lint run)
        # Capture the exit code
        lint_exit_code=$?
        if [ $lint_exit_code -ne 0 ]; then
            echo "golangci-lint failed in $dir"
            exit $lint_exit_code
        fi
    else
        echo "No modified Go files in $dir"
    fi
done
