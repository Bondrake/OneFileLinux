;;;; OneFileLinux Dry Run Fix
;;;; This file manually loads the system and patches the ASDF registry

;; First, load ASDF
(require :asdf)

;; Explicitly register the system in the current directory
(let ((base-dir (uiop:getcwd)))
  (push base-dir asdf:*central-registry*)
  
  ;; Load the system definition
  (format t "~%Loading system from: ~A~%" base-dir)
  (load (merge-pathnames "onefilelinux.asd" base-dir))
  
  ;; Load the system
  (format t "Loading onefilelinux system...~%")
  (asdf:load-system "onefilelinux"))

;; Now run from main
(format t "~%Running OneFileLinux build system...~%")
(let ((args (uiop:command-line-arguments)))
  (handler-case
      (progn
        (onefilelinux.main::main)
        (uiop:quit 0))
    (error (e)
      (format *error-output* "ERROR: ~A~%" e)
      (uiop:quit 1))))