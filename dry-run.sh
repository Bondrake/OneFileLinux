#!/bin/bash
# OneFileLinux Dry Run Test Script

# Get the directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
cd "$SCRIPT_DIR"

# Run SBCL with the fix-dry-run.lisp file
sbcl --load "fix-dry-run.lisp" -- --dry-run "$@"