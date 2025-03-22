;;;; OneFileLinux Dry Run Fix
;;;; This file manually loads the system to support dry run functionality

;; First, load ASDF
(require :asdf)

;; Ensure Quicklisp is loaded and dependencies are available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" 
                                     (user-homedir-pathname))))
  (if (probe-file quicklisp-init)
      (progn
        (format t "~%Loading Quicklisp...~%")
        (load quicklisp-init)
        
        ;; Make sure required libraries are loaded first
        (format t "~%Ensuring dependencies are loaded...~%")
        (let ((*standard-output* *standard-output*)) ; Keep output visible
          (funcall (read-from-string "ql:quickload") :uiop)
          (funcall (read-from-string "ql:quickload") :cl-ppcre)
          (funcall (read-from-string "ql:quickload") :alexandria))
        (format t "Dependencies loaded successfully~%"))
      (format t "~%WARNING: Quicklisp not found at ~A~%" quicklisp-init)))

;; Explicitly register the system in the current directory
(let ((base-dir (uiop:getcwd)))
  ;; Add current directory to ASDF search paths
  (push base-dir asdf:*central-registry*)
  
  ;; Load the system definition
  (format t "~%Loading system from: ~A~%" base-dir)
  (handler-case
      (progn
        ;; First try loading the ASD file directly
        (load (merge-pathnames "onefilelinux.asd" base-dir))
        
        ;; Then load the system with ASDF
        (format t "Loading onefilelinux system...~%")
        (asdf:load-system "onefilelinux"))
    (error (e)
      (format t "~%Error during system loading: ~A~%" e)
      (format t "~%Attempting manual file loading...~%")
      
      ;; If ASDF fails, try manual loading of essential files
      (load (merge-pathnames "core.lisp" base-dir))
      (load (merge-pathnames "config.lisp" base-dir))
      (load (merge-pathnames "build.lisp" base-dir))
      
      ;; Load step definitions
      (load (merge-pathnames "steps/prepare.lisp" base-dir))
      (load (merge-pathnames "steps/get.lisp" base-dir))
      (load (merge-pathnames "steps/chrootandinstall.lisp" base-dir))
      (load (merge-pathnames "steps/conf.lisp" base-dir))
      (load (merge-pathnames "steps/build.lisp" base-dir))
      
      ;; Load main file
      (load (merge-pathnames "main.lisp" base-dir)))))

;; Now run the system with command-line arguments
(format t "~%Running OneFileLinux build system...~%")
(let ((args (uiop:command-line-arguments)))
  (handler-case
      (progn
        (funcall (read-from-string "onefilelinux.main:main"))
        (uiop:quit 0))
    (error (e)
      (format *error-output* "ERROR: ~A~%" e)
      (uiop:quit 1))))