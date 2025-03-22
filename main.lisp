;;;; OneFileLinux Main Entry Point
;;;; Main script for loading the system and processing command-line arguments

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

;; Note: We're using sb-mop which is built into SBCL, no need to require or load it

;; Now load the OneFileLinux system
(format t "Loading OneFileLinux system...~%")
(funcall (read-from-string "ql:quickload") "onefilelinux" :verbose t)

(defpackage :onefilelinux.main
  (:use :cl :onefilelinux.core :onefilelinux.build)
  (:export #:main
           #:display-banner))

(in-package :onefilelinux.main)

(defun display-banner ()
  "Display the OneFileLinux banner"
  (format t "~%")
  (format t "  ┌───────────────────────────────────────┐~%")
  (format t "  │             OneFileLinux              │~%")
  (format t "  │     Refactored Build System v1.0      │~%")
  (format t "  └───────────────────────────────────────┘~%~%"))

(defun display-version ()
  "Display version information"
  (format t "OneFileLinux Build System v1.0.0~%")
  (format t "Common Lisp Implementation: ~A ~A~%" 
          (lisp-implementation-type) 
          (lisp-implementation-version))
  (format t "Operating System: ")
  (multiple-value-bind (os-name os-version os-type)
      (onefilelinux.core:detect-os)
    (format t "~A ~A (~A)~%" os-name os-version os-type))
  (format t "~%"))

(defun main (&optional args)
  "Main entry point for the build system"
  (display-banner)
  
  ;; Process command-line arguments
  (let ((args (or args (uiop:command-line-arguments))))
    ;; Check for version and help flags first
    (when (member "--version" args :test #'string=)
      (display-version)
      (uiop:quit 0))
    
    (when (or (member "--help" args :test #'string=)
              (member "-h" args :test #'string=)
              (null args))
      (onefilelinux.build:print-usage)
      (uiop:quit 0))
    
    ;; Set up logging based on command-line options
    (when (member "--debug" args :test #'string=)
      (setf *log-level* :debug))
    
    (when (member "--quiet" args :test #'string=)
      (setf *log-level* :warning))
    
    ;; Handle explicit log file
    (let ((log-file-arg (find-if (lambda (arg) 
                                  (onefilelinux.core:string-prefix-p "--log-file=" arg))
                                args)))
      (when log-file-arg
        (setf *log-file* (subseq log-file-arg 11))))
    
    ;; Execute main build function
    (handler-case
        (progn
          (log-message :info "Starting OneFileLinux build process with args: ~S" args)
          ;; Ensure we're passing a proper argument list
          (if (listp args)
              (onefilelinux.build:main args)
              (onefilelinux.build:main nil))
          (log-message :info "Build process completed successfully")
          (uiop:quit 0))
      
      (onefilelinux.core:onefilelinux-error (e)
        (log-message :error "Build failed: ~A" (onefilelinux.core:error-message e))
        (format *error-output* "ERROR: ~A~%" (onefilelinux.core:error-message e))
        (uiop:quit 1))
      
      (error (e)
        (log-message :error "Unexpected error: ~A" e)
        (format *error-output* "FATAL ERROR: ~A~%" e)
        (uiop:quit 2)))))

;; Execute main function when script is loaded directly, but only if not loaded from another script
(eval-when (:execute)
  ;; Only run main if loaded directly via --load, not when loaded as part of a system
  (when (or (member "--" (uiop:command-line-arguments) :test #'string=)
            (and (> (length (uiop:command-line-arguments)) 0)
                 (not (find-package "ONEFILELINUX"))))
    (main)))