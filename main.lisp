;;;; OneFileLinux Main Entry Point
;;;; Main script for loading the system and processing command-line arguments

(require :asdf)

;; Ensure proper dependency loading before loading the ASDF system
(defun ensure-dependency (name)
  (handler-case
      (progn
        (format t "Loading dependency: ~A~%" name)
        (asdf:load-system name))
    (error (e)
      (format t "Warning: Failed to load ~A: ~A~%" name e)
      nil)))

;; Load essential dependencies first
(ensure-dependency :uiop)
(ensure-dependency :cl-ppcre)
(ensure-dependency :alexandria)

;; Now load the OneFileLinux system
(asdf:load-system "onefilelinux")

(defpackage :onefilelinux.main
  (:use :cl :onefilelinux.core :onefilelinux.build)
  (:export #:main))

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

;; Execute main function when script is loaded directly
(eval-when (:execute)
  (main))