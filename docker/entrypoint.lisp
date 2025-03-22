;;;; OneFileLinux Docker Entrypoint
;;;; Entry point for Docker container execution
(require :asdf)
(asdf:load-system "onefilelinux")

(defpackage :onefilelinux.docker.entrypoint
  (:use :cl :onefilelinux.core :onefilelinux.config :onefilelinux.docker.build)
  (:export #:main))

(in-package :onefilelinux.docker.entrypoint)

(defun prepare-docker-environment (config)
  "Prepare the Docker environment for building"
  (log-message :info "Preparing Docker build environment")
  
  ;; Create required directories
  (let ((output-dir (config-value config :docker :output-dir "/output"))
        (log-file (config-value config :docker :log-file "/var/log/onefilelinux-build.log")))
    
    ;; Ensure output directory exists
    (ensure-directory output-dir)
    (run-command (format nil "chmod 777 ~A" output-dir))
    
    ;; Ensure log file directory exists
    (ensure-directory (directory-namestring log-file))
    (run-command (format nil "touch ~A" log-file))
    (run-command (format nil "chmod 666 ~A" log-file)))
  
  ;; Detect available resources if auto-detection is enabled
  (when (config-value config :docker :auto-detect-resources t)
    (setup-resource-limits config))
  
  (log-message :info "Docker environment prepared successfully"))

(defun setup-resource-limits (config)
  "Set up resource limits for the Docker container"
  (log-message :debug "Setting up resource limits")
  
  (let* ((detection-result (onefilelinux.docker.resources:detect-resources))
         (cpu-count (onefilelinux.docker.resources:resource-cpu-count detection-result))
         (memory-mb (onefilelinux.docker.resources:resource-memory-mb detection-result))
         (cpu-limit (onefilelinux.docker.resources:resource-cpu-limit detection-result))
         (memory-limit (onefilelinux.docker.resources:resource-memory-limit detection-result))
         
         ;; Use detected limits or defaults
         (effective-cpu (or cpu-limit 
                           (config-value config :docker :default-cpu-limit 2)))
         (effective-memory (or memory-limit 
                              (config-value config :docker :default-memory-limit 4096))))
    
    ;; Set resource limits in configuration
    (log-message :info "Setting resource limits: ~A CPUs, ~A MB memory" 
               effective-cpu effective-memory)
    
    (setf (config-value config :build :make-jobs) effective-cpu)
    (setf (config-value config :docker :memory-limit) effective-memory)
    
    ;; Set environment variables to influence child processes
    (setenv "MAKEFLAGS" (format nil "-j~D" effective-cpu))
    (setenv "MALLOC_ARENA_MAX" "2")  ; Reduce memory fragmentation
    
    ;; Return resources
    (list :cpu effective-cpu :memory effective-memory)))

(defun main (args)
  "Main entry point for Docker entrypoint"
  (handler-case
      (let ((config (make-default-config)))
        ;; Set up logging
        (setf *log-level* :info)
        (setf *log-file* (config-value config :docker :log-file "/var/log/onefilelinux-build.log"))
        
        ;; Prepare Docker environment
        (prepare-docker-environment config)
        
        ;; Run the Docker build with the given arguments
        (onefilelinux.docker.build:main args))
    
    (error (e)
      (format t "ERROR: ~A~%" e)
      (uiop:quit 1))))

;; Run main when this file is loaded
(eval-when (:execute)
  (main uiop:*command-line-arguments*))