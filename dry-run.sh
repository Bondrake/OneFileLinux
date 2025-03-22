#!/bin/bash
# OneFileLinux Dry Run Test Script

# Get the directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
cd "$SCRIPT_DIR"

# Create a simple loader that loads dependencies and then passes control to main.lisp
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

;; Create a modified main.lisp with auto-execution disabled
(with-open-file (out "temp-main.lisp" :direction :output :if-exists :supersede)
  ;; Copy the original file, but disable auto-execution
  (with-open-file (in "main.lisp")
    (loop for line = (read-line in nil nil)
          while line
          do (if (and (search "(eval-when (:execute)" line)
                       (search "main" line))
                 ;; Replace auto-execution line with a no-op version
                 (write-line "(eval-when (:execute) nil)" out)
                 ;; Otherwise keep the line as is
                 (write-line line out)))))

;; Now load the modified main file
(format t "Loading main system...~%")
(load "temp-main.lisp")

;; Display banner and run in dry-run mode
(format t "~%Running OneFileLinux in dry-run mode...~%~%")

;; Create the argument list with --dry-run first
(let ((args (list* "--dry-run" (copy-list (uiop:command-line-arguments)))))
  ;; Call the main function with our arguments
  (funcall (read-from-string "funcall") 
           (read-from-string "onefilelinux.main:main") 
           args))
EOF

# Run the dry-run loader
sbcl --noinform --load "dry-run-loader.lisp" -- "$@"

# Clean up
rm -f dry-run-loader.lisp temp-main.lisp