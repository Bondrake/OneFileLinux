;;;; OneFileLinux Docker Build Launcher
;;;; Main entry point for building OneFileLinux in a Docker environment
(defpackage :onefilelinux.docker.build
  (:use :cl :onefilelinux.core :onefilelinux.config :onefilelinux.build)
  (:export #:main
           #:build-with-resources
           #:parse-docker-arguments))

(in-package :onefilelinux.docker.build)

;;; ----------------------------------------------------
;;; Docker Build Configuration
;;; ----------------------------------------------------

(defclass docker-build-config ()
  ((build-config :initarg :build-config
                :accessor docker-build-config
                :documentation "Core build configuration")
   (resources :initarg :resources
             :accessor docker-build-resources
             :initform nil
             :documentation "Resource allocation configuration")
   (github-actions :initarg :github-actions
                  :accessor docker-build-github-actions
                  :initform nil
                  :documentation "GitHub Actions integration settings"))
  (:documentation "Docker-specific build configuration with resource allocation."))

;;; ----------------------------------------------------
;;; Main Entry Point
;;; ----------------------------------------------------

(defun main ()
  "Main entry point for Docker-based OneFileLinux build."
  (handler-case
      (let* ((argv (uiop:command-line-arguments))
             (docker-config (parse-docker-arguments argv)))
        
        ;; Set up logging with appropriate destinations
        (setup-docker-logging docker-config)
        
        ;; Log build start
        (log-message :info "Starting OneFileLinux build in Docker environment")
        
        ;; Configure resources based on detection or settings
        (let ((resources (or (docker-build-resources docker-config)
                            (detect-available-resources))))
          (setf (docker-build-resources docker-config) resources)
          (log-message :info "Using resources: ~A cores, ~A MB memory" 
                      (getf resources :cpu) (getf resources :memory)))
        
        ;; Set up GitHub Actions integration if enabled
        (when (getf (docker-build-github-actions docker-config) :enabled)
          (setup-github-actions docker-config))
        
        ;; Run the build with configured resources
        (build-with-resources docker-config)
        
        ;; Return success
        (log-message :info "Docker build completed successfully")
        0)
    
    (onefilelinux-error (err)
      ;; Log the error and exit with failure code
      (log-message :error "Build failed: ~A" (error-message err))
      (when (getf (docker-build-github-actions docker-config) :enabled)
        (report-github-failure err))
      1)
    
    (error (err)
      ;; Log unexpected errors and exit with failure code
      (log-message :error "Unexpected error: ~A" err)
      (when (getf (docker-build-github-actions docker-config) :enabled)
        (report-github-failure err))
      2)))

;;; ----------------------------------------------------
;;; Docker-Specific Build Functions
;;; ----------------------------------------------------

(defun build-with-resources (docker-config)
  "Run the OneFileLinux build with resource constraints."
  (let* ((config (docker-build-config docker-config))
         (resources (docker-build-resources docker-config))
         (cpu-limit (getf resources :cpu))
         (memory-limit (getf resources :memory)))
    
    ;; Configure build with resource limits
    (setf (config-value config :build :make-jobs cpu-limit)
          (max 1 (floor cpu-limit)))
    
    ;; Adjust memory-intensive operations based on available memory
    (when (< memory-limit 4096)
      (log-message :warning "Limited memory detected (~D MB). Adjusting build parameters." memory-limit)
      (setf (config-value config :build :minimize-kernel t))
      (setf (config-value config :build :skip-custom-packages t)))
    
    ;; Setup GitHub workflow tracing if enabled
    (let ((github-config (docker-build-github-actions docker-config)))
      (when (and github-config (getf github-config :enabled))
        (setf (config-value config :build :github-actions t))
        (setup-github-build-tracing config)))
    
    ;; Run the build process
    (with-resource-monitoring resources
      (onefilelinux.build:run-build config))))

(defun parse-docker-arguments (argv)
  "Parse command-line arguments for the Docker build environment."
  (let* ((config (make-default-config))
         (docker-config (make-instance 'docker-build-config
                                     :build-config config))
         (current-arg nil))
    
    ;; Set defaults from configuration
    (let ((default-cpu (config-value config :docker :default-cpu-limit 2))
          (default-memory (config-value config :docker :default-memory-limit 4096))
          (resources (list :cpu default-cpu :memory default-memory)))
      (setf (docker-build-resources docker-config) resources))
    
    ;; Process command line arguments
    (loop for arg in argv
          do (cond
               ;; Process --profile
               ((string= arg "--profile")
                (setf current-arg :profile))
               
               ;; Process --cpu
               ((string= arg "--cpu")
                (setf current-arg :cpu))
               
               ;; Process --memory
               ((string= arg "--memory")
                (setf current-arg :memory))
               
               ;; Process --github-actions
               ((string= arg "--github-actions")
                (setf (docker-build-github-actions docker-config)
                      (list :enabled t))
                (setf current-arg nil))
               
               ;; Process --output-dir
               ((string= arg "--output-dir")
                (setf current-arg :output-dir))
               
               ;; Process --log-file
               ((string= arg "--log-file")
                (setf current-arg :log-file))
               
               ;; Process --help
               ((or (string= arg "--help") (string= arg "-h"))
                (display-help)
                (exit 0))
               
               ;; Process value for current argument
               (current-arg
                (case current-arg
                  (:profile
                   (setf (config-value (docker-build-config docker-config) :build :profile arg)))
                  
                  (:cpu
                   (let ((resources (docker-build-resources docker-config)))
                     (setf (getf resources :cpu) (parse-integer arg))
                     (setf (docker-build-resources docker-config) resources)))
                  
                  (:memory
                   (let ((resources (docker-build-resources docker-config)))
                     (setf (getf resources :memory) (parse-integer arg))
                     (setf (docker-build-resources docker-config) resources)))
                  
                  (:output-dir
                   (setf (config-value (docker-build-config docker-config) :build :output-dir arg)))
                  
                  (:log-file
                   (setf (config-value (docker-build-config docker-config) :docker :log-file arg))))
                
                (setf current-arg nil))
               
               ;; Unknown argument
               (t
                (log-message :warning "Unknown argument: ~A" arg))))
    
    docker-config))

;;; ----------------------------------------------------
;;; Resource Management
;;; ----------------------------------------------------

(defun detect-available-resources ()
  "Detect available CPU and memory resources in the Docker environment."
  (log-message :debug "Detecting available system resources")
  
  (let ((cpu-count (detect-cpu-count))
        (memory-mb (detect-memory-mb)))
    
    (log-message :debug "Detected ~D CPU cores and ~D MB of memory" cpu-count memory-mb)
    
    (list :cpu cpu-count
          :memory memory-mb)))

(defun detect-cpu-count ()
  "Detect the number of available CPU cores."
  (let ((result (run-command-output "nproc" '())))
    (parse-integer (string-trim '(#\Space #\Tab #\Newline) result))))

(defun detect-memory-mb ()
  "Detect the amount of available memory in MB."
  (let* ((mem-info (run-command-output "cat" '("/proc/meminfo")))
         (total-line (find-if (lambda (line)
                               (prefixp "MemTotal:" line))
                             (split-string mem-info #\Newline)))
         (mem-kb (parse-integer (regex-replace-all "[^0-9]" total-line ""))))
    (floor mem-kb 1024)))

(defmacro with-resource-monitoring ((resources) &body body)
  "Execute body with resource monitoring."
  `(progn
     ;; Start resource monitoring
     (let ((monitor-thread (start-resource-monitoring ,resources)))
       (unwind-protect
           (progn ,@body)
         
         ;; Stop resource monitoring
         (when monitor-thread
           (stop-resource-monitoring monitor-thread))))))

(defun start-resource-monitoring (resources)
  "Start a thread to monitor resource usage."
  (log-message :debug "Starting resource usage monitoring")
  
  ;; In a real implementation, this would spawn a thread
  ;; Here we just return a placeholder
  nil)

(defun stop-resource-monitoring (monitor-thread)
  "Stop the resource monitoring thread."
  (log-message :debug "Stopping resource usage monitoring")
  
  ;; In a real implementation, this would terminate the monitoring thread
  nil)

;;; ----------------------------------------------------
;;; GitHub Actions Integration
;;; ----------------------------------------------------

(defun setup-github-actions (docker-config)
  "Set up GitHub Actions integration."
  (log-message :info "Setting up GitHub Actions integration")
  
  ;; Configure output for GitHub Actions
  (let ((github-config (docker-build-github-actions docker-config)))
    (setf (getf github-config :output-format) "github")
    (setf (getf github-config :create-annotations) t)))

(defun setup-github-build-tracing (config)
  "Set up build tracing for GitHub Actions."
  (log-message :debug "Setting up GitHub Actions build tracing")
  
  ;; Add GitHub step tracing
  (setf (config-value config :build :trace-steps t))
  (setf (config-value config :build :trace-output-format "github")))

(defun report-github-failure (error)
  "Report build failure to GitHub Actions."
  (log-message :debug "Reporting failure to GitHub Actions")
  
  ;; In a real implementation, this would use GitHub Actions annotations
  (format t "::error::Build failed: ~A~%" (if (typep error 'onefilelinux-error)
                                            (error-message error)
                                            error)))

;;; ----------------------------------------------------
;;; Utility Functions
;;; ----------------------------------------------------

(defun setup-docker-logging (docker-config)
  "Set up logging for Docker environment."
  ;; Configure logging to output to both console and file
  (setf *log-destinations* (list :console :file))
  (setf *log-file* "/var/log/onefilelinux-build.log")
  
  ;; Set log level based on environment variable
  (let ((log-level (uiop:getenv "ONEFILELINUX_LOG_LEVEL")))
    (when log-level
      (setf *log-level* (parse-log-level log-level)))))

(defun parse-log-level (level-string)
  "Parse a log level string into a keyword."
  (cond
    ((string-equal level-string "debug") :debug)
    ((string-equal level-string "info") :info)
    ((string-equal level-string "warning") :warning)
    ((string-equal level-string "error") :error)
    (t :info)))

(defun display-help ()
  "Display help information for the Docker build script."
  (format t "OneFileLinux Docker Build~%")
  (format t "~%")
  (format t "Usage: onefilelinux-build [options]~%")
  (format t "~%")
  (format t "Options:~%")
  (format t "  --profile PROFILE    Build profile to use (minimal, standard, full)~%")
  (format t "  --cpu NUM            Number of CPU cores to use~%")
  (format t "  --memory NUM         Memory limit in MB~%")
  (format t "  --github-actions     Enable GitHub Actions integration~%")
  (format t "  --output-dir DIR     Directory for build output~%")
  (format t "  --help, -h           Display this help message~%")
  (format t "~%")
  (format t "Environment variables:~%")
  (format t "  ONEFILELINUX_LOG_LEVEL   Set log level (debug, info, warning, error)~%"))

(defun prefixp (prefix string)
  "Check if string starts with prefix."
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

;;; ----------------------------------------------------
;;; Default Configuration
;;; ----------------------------------------------------

(defun make-default-config ()
  "Create a default configuration for Docker builds."
  (let ((config (make-instance 'configuration)))
    ;; Set Docker-specific defaults
    (setf (config-value config :build :working-dir) "/build")
    (setf (config-value config :build :output-dir) "/output")
    (setf (config-value config :build :profile) "minimal")
    
    ;; Enable default features
    (setf (config-value config :features :enabled) '("crypto-support" "network-tools"))
    
    config))