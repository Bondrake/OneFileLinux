#!/bin/bash
# OneFileLinux Dry Run Until Step Script

# Usage check
if [ -z "$1" ]; then
  echo "Usage: $0 <step>"
  echo "Available steps: prepare, get, chroot, conf, build"
  exit 1
fi

# Get the directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
cd "$SCRIPT_DIR"

# Run SBCL with the fix-dry-run.lisp file
sbcl --load "fix-dry-run.lisp" -- --dry-run-until="$1" "${@:2}"