#!/bin/bash
# OneFileLinux Dry Run Test Script

# Get the directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
cd "$SCRIPT_DIR"

# Run regular build system with dry-run flag
sbcl --load "main.lisp" -- --dry-run "$@"