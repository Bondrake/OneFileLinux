;;;; OneFileLinux Build Step: Prepare
;;;; Environment preparation and system detection for the build process

(defpackage :onefilelinux.build.prepare
  (:use :cl :onefilelinux.core :onefilelinux.config :onefilelinux.build)
  (:export #:make-prepare-step
           #:register-prepare-step))

(in-package :onefilelinux.build.prepare)

;;; ===============================
;;; Prepare Step Implementation
;;; ===============================

(defclass prepare-step (build-step)
  ()
  (:default-initargs 
   :name :prepare
   :description "Prepare build environment and dependencies"
   :dependencies nil)
  (:documentation "Build step for environment preparation"))

(defmethod prepare ((step prepare-step) context)
  (log-message :info "Preparing build environment")
  
  ;; Check privileges first
  (check-privileges context)
  
  ;; Create required directories
  (create-directories context)
  
  ;; Detect system environment
  (detect-system-environment context)
  
  ;; Check build requirements
  (check-build-requirements context)
  
  t)

(defmethod execute ((step prepare-step) context)
  (log-message :info "Executing environment preparation")
  
  ;; Install required dependencies
  (install-dependencies context)
  
  ;; Setup cache if enabled
  (when (config-value 'build-config 'use-cache)
    (setup-cache context))
  
  ;; Verify scripts and files
  (verify-files context)
  
  t)

(defmethod cleanup ((step prepare-step) context status)
  (log-message :info "Cleaning up after preparation")
  
  ;; Nothing to clean here
  t)

;;; ===============================
;;; Helper Functions
;;; ===============================

(defun create-directories (context)
  "Create required directories for the build"
  (log-message :info "Creating required directories")
  
  ;; Get artifact paths from configuration
  (let ((paths (config-value 'system-config 'artifact-paths)))
    ;; Create each directory
    (loop for (key path) on paths by #'cddr
          do (let ((full-path (path-join (build-context-working-dir context) path)))
               (log-message :debug "Creating directory: ~A" full-path)
               (ensure-directory full-path)))))

(defun detect-system-environment (context)
  "Detect the current system environment"
  (log-message :info "Detecting system environment")
  
  ;; Detect OS and version
  (multiple-value-bind (os-name os-version os-type)
      (detect-os)
    (log-message :info "Detected: ~A ~A (~A)" os-name os-version os-type)
    
    ;; Detect system resources
    (let ((cpus (detect-cpus))
          (memory-bytes (detect-memory))
          (memory-gb (/ (detect-memory) 1024 1024 1024.0)))
      (log-message :info "System resources: ~D CPUs, ~,1fGB memory" 
                 cpus memory-gb)
      
      ;; Update configuration
      (unless (config-value 'build-config 'build-jobs)
        (set-config-value 'build-config 'build-jobs cpus)))))

(defun check-privileges (context)
  "Check if running with sufficient privileges"
  (log-message :info "Checking privileges")
  
  (with-slots (config) context
    (let ((is-root (zerop (parse-integer 
                           (string-trim '(#\Space #\Tab #\Newline)
                                       (shell-command-to-string "id -u")))))
          (require-root (config-value config :build-requirements :require-root t)))
      
      ;; Log privilege status
      (if is-root
          (log-message :debug "Running with root privileges")
          (log-message :debug "Running without root privileges"))
      
      ;; Check if root is required
      (when (and require-root (not is-root))
        (log-message :warning "Not running as root. Some operations may fail.")
        
        ;; Throw error if strict mode is enabled
        (when (config-value config :build-requirements :strict-root-check nil)
          (error 'onefilelinux-error
                :message "Root privileges required for build operations. Please run as root or with sudo."))))
    
    ;; Verify output directory is writable
    (ensure-writable-directory (build-context-output-dir context))))

(defun ensure-writable-directory (path)
  "Ensure directory exists and is writable"
  (log-message :debug "Ensuring directory is writable: ~A" path)
  (ensure-directory path)
  (unless (run-command-status (format nil "test -w ~A" path))
    (error 'file-system-error
           :message "Directory not writable"
           :path path)))

(defun check-build-requirements (context)
  "Check if build requirements are met"
  (log-message :info "Checking build requirements")
  
  (with-slots (config) context
    (let* ((required-tools (config-value config :build-requirements :required-tools))
           (chroot-tools (when (config-value config :build :use-chroot t)
                          (config-value config :build-requirements :chroot-tools)))
           (download-tools (when (config-value config :build :download-sources t)
                           (config-value config :build-requirements :download-tools)))
           (all-required-tools (append required-tools chroot-tools download-tools))
           (missing-tools '()))
      
      ;; Check for each required tool
      (dolist (tool all-required-tools)
        (unless (which tool)
          (push tool missing-tools)))
      
      ;; Report results
      (cond
        ((null missing-tools)
         (log-message :success "All required build tools available"))
        
        ((config-value config :build-requirements :strict-tool-check nil)
         (error 'onefilelinux-error
                :message (format nil "Missing required build tools: ~{~A~^, ~}" 
                               (nreverse missing-tools))))
        
        (t
         (log-message :warning "Missing some build tools: ~{~A~^, ~}" 
                    (nreverse missing-tools))
         (log-message :warning "Continuing anyway as strict tool checking is disabled")))
      
      ;; Return number of missing tools (0 = success)
      (length missing-tools))))

(defun install-dependencies (context)
  "Install required dependencies based on detected OS"
  (log-message :info "Installing required dependencies")
  
  ;; Determine OS type and package list
  (multiple-value-bind (os-name os-version os-type)
      (detect-os)
    
    ;; Install packages based on OS type
    (case (intern (string-upcase os-type) "KEYWORD")
      (:linux
       (install-linux-dependencies os-name))
      
      (:darwin
       (install-macos-dependencies))
      
      (otherwise
       (log-message :warning "Unsupported OS type: ~A. Manual dependency installation required." 
                  os-type)))))

(defun install-linux-dependencies (os-name)
  "Install dependencies on Linux systems"
  (let ((package-cmd
         (cond
           ((or (string= os-name "Ubuntu") 
                (string= os-name "Debian") 
                (string= os-name "Linux Mint"))
            "apt-get update && apt-get install -y")
           
           ((or (string= os-name "Fedora") 
                (string= os-name "CentOS") 
                (string= os-name "RHEL"))
            (if (which "dnf")
                "dnf install -y"
                "yum install -y"))
           
           ((or (string= os-name "Arch Linux")
                (string= os-name "Manjaro Linux"))
            "pacman -Sy --noconfirm")
           
           ((string= os-name "Alpine Linux")
            "apk add")
           
           (t nil))))
    
    (if package-cmd
        ;; Get package list for OS
        (let ((packages '("wget" "tar" "xz-utils" "make" "gcc" "g++" "ld" 
                        "libssl-dev" "bc" "flex" "bison")))
          (log-message :info "Installing packages: ~{~A~^, ~}" packages)
          
          ;; Run package command
          (let ((cmd (format nil "~A ~{~A~^ ~}" package-cmd packages)))
            (log-message :debug "Running: ~A" cmd)
            (run-command cmd)))
        
        (log-message :warning "Unknown Linux distribution. Manual dependency installation required."))))

(defun install-macos-dependencies ()
  "Install dependencies on macOS systems"
  (log-message :info "Checking macOS dependencies")
  
  ;; Check for Homebrew
  (if (which "brew")
      (progn
        (log-message :info "Installing dependencies with Homebrew")
        (run-command "brew install wget coreutils gnu-tar xz make gcc"))
      
      (log-message :warning "Homebrew not found. Please install Homebrew and required dependencies manually.")))

(defun setup-cache (context)
  "Set up caching directories"
  (log-message :info "Setting up build cache")
  
  (let ((cache-dir (config-value 'build-config 'cache-dir)))
    (ensure-directory cache-dir)
    
    ;; Create cache subdirectories
    (dolist (subdir '("sources" "ccache" "packages" "build"))
      (ensure-directory (path-join cache-dir subdir)))
    
    (log-message :success "Cache enabled at: ~A" cache-dir)))

(defun verify-files (context)
  "Verify required files and scripts exist"
  (log-message :info "Verifying required files")
  
  ;; Check core files
  (let ((missing-files '()))
    (dolist (file '("build.lisp" "config.lisp" "core.lisp"))
      (unless (file-exists-p (path-join (build-context-working-dir context) file))
        (push file missing-files)))
    
    (if missing-files
        (progn
          (log-message :error "Missing required files: ~{~A~^, ~}" 
                     (nreverse missing-files))
          (error 'file-system-error
                :message "Missing required files"
                :path (build-context-working-dir context)))
        (log-message :success "All required files verified"))))

;;; ===============================
;;; Step Registration
;;; ===============================

(defun make-prepare-step ()
  "Create a new prepare step instance"
  (make-instance 'prepare-step))

(defun register-prepare-step ()
  "Register the prepare step with the build system"
  (register-build-step (make-prepare-step)))

;; Register the step when this file is loaded
(register-prepare-step)