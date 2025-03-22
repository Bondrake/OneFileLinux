#!/bin/bash
# OneFileLinux Dry Run Test Script

# Get the directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
cd "$SCRIPT_DIR"

# Disable the automatic loading
cat > dry-run-loader.lisp << 'EOF'
(require :asdf)

;; Load Quicklisp - build.sh should ensure this is set up
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (progn
        (format t "Loading Quicklisp...~%")
        (load quicklisp-init))
      (error "Quicklisp not found at ~A. Please run build.sh first to set up the build environment." quicklisp-init)))

;; Register the current directory with ASDF
(push *default-pathname-defaults* asdf:*central-registry*)

;; Suppress style warnings in SBCL to focus on errors
#+sbcl
(progn
  ;; Muffle style warnings in the compiler
  (setf sb-ext:*muffled-warnings* 'style-warning)
  
  ;; Try to find ASDF variable if it exists (may not exist in all SBCL versions)
  (when (find-symbol "*SUPPRESS-COMPILATION-WARNINGS*" :asdf)
    (setf (symbol-value (find-symbol "*SUPPRESS-COMPILATION-WARNINGS*" :asdf)) t)))

;; Load essential dependencies using Quicklisp
(format t "Loading dependencies using Quicklisp...~%")
(funcall (read-from-string "ql:quickload") :uiop :verbose t)
(funcall (read-from-string "ql:quickload") :cl-ppcre :verbose t)
(funcall (read-from-string "ql:quickload") :alexandria :verbose t)

;; Now load the OneFileLinux system
(format t "Loading OneFileLinux system...~%")
(funcall (read-from-string "ql:quickload") "onefilelinux" :verbose t)

;; Call the main function with --dry-run
(format t "Running dry-run mode...~%")
(funcall (read-from-string "onefilelinux.main:display-banner"))
(let ((args (append (list "--dry-run") (uiop:command-line-arguments))))
  (funcall (read-from-string "onefilelinux.build:main") args))
EOF

# Run the dry-run loader
sbcl --noinform --load "dry-run-loader.lisp" -- "$@"

# Clean up
rm dry-run-loader.lisp