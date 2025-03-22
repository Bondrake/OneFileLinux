;;;; OneFileLinux Build System
;;;; Core orchestration for the build process using command pattern

(defpackage :onefilelinux.build
  (:use :cl :onefilelinux.core :onefilelinux.config)
  (:export
   ;; Main entry point
   #:main
   
   ;; Build step protocol
   #:build-step
   #:build-step-name
   #:build-step-description
   #:build-step-dependencies
   #:register-build-step
   #:get-build-step
   #:list-build-steps
   
   ;; Build context
   #:make-build-context
   #:build-context
   #:build-context-config
   #:build-context-working-dir
   #:build-context-output-dir
   #:build-context-step
   #:build-context-args
   #:build-context-start-time
   #:build-context-verbose
   #:build-context-timing
   #:build-context-dry-run
   #:build-context-dry-run-until
   
   ;; Build execution
   #:execute-build-step
   #:run-build
   #:parse-arguments))

(in-package :onefilelinux.build)

;;; ===============================
;;; Build Step Protocol
;;; ===============================

(defclass build-step ()
  ((name :initarg :name
        :accessor build-step-name
        :documentation "Name of the build step")
   (description :initarg :description
               :accessor build-step-description
               :documentation "Description of the build step")
   (dependencies :initarg :dependencies
                :accessor build-step-dependencies
                :initform nil
                :documentation "List of build step dependencies"))
  (:documentation "Base class for build steps"))

;; Method protocol for build steps
(defgeneric prepare (step context)
  (:documentation "Prepare the environment for a build step"))

(defgeneric execute (step context)
  (:documentation "Execute the build step"))

(defgeneric cleanup (step context status)
  (:documentation "Clean up after a build step"))

;; Registry for build steps
(defparameter *build-steps* (make-hash-table :test 'eq)
  "Registry of build steps")

(defun register-build-step (step)
  "Register a build step"
  (setf (gethash (build-step-name step) *build-steps*) step))

(defun get-build-step (name)
  "Get a build step by name"
  (or (gethash name *build-steps*)
      (error 'configuration-error
            :message (format nil "Unknown build step: ~A" name)
            :key name)))

(defun list-build-steps ()
  "Get a list of all build steps"
  (let ((steps '()))
    (maphash (lambda (name step)
               (declare (ignore name))
               (push step steps))
             *build-steps*)
    (sort steps #'string< :key (lambda (step) (symbol-name (build-step-name step))))))

;;; ===============================
;;; Build Context
;;; ===============================

(defstruct build-context
  "Context for a build run"
  (config nil :type hash-table)
  (working-dir "" :type string)
  (output-dir "" :type string)
  (step nil)
  (args nil :type list)
  (start-time 0 :type integer)
  (verbose nil :type boolean)
  (timing (make-hash-table :test 'eq) :type hash-table)
  (dry-run nil :type boolean)
  (dry-run-until nil :type (or symbol null)))

;;; ===============================
;;; Build Execution
;;; ===============================

(defun execute-build-step (step-name context)
  "Execute a single build step"
  (let* ((step (get-build-step step-name))
         (step-start-time (get-universal-time))
         (status nil))
    
    ;; Update context
    (setf (build-context-step context) step-name)
    
    ;; Log start of step
    (log-message :info "Starting build step: ~A - ~A" 
               step-name (build-step-description step))
    
    (with-logging-context (step-name)
      (handler-case
          (progn
            ;; Prepare step
            (log-message :debug "Preparing build step")
            (prepare step context)
            
            ;; Execute step
            (log-message :debug "Executing build step")
            (setf status (execute step context))
            
            ;; Cleanup step
            (log-message :debug "Cleaning up build step")
            (cleanup step context status))
        
        (error (e)
          (log-message :error "Build step failed: ~A" e)
          (setf status nil))))
    
    ;; Log timing
    (let ((step-end-time (get-universal-time))
          (step-duration (- (get-universal-time) step-start-time)))
      (setf (gethash step-name (build-context-timing context)) step-duration)
      (log-message :info "Completed build step ~A in ~A seconds" 
                 step-name step-duration))
    
    ;; Return status
    status))

(defun get-step-execution-order (steps)
  "Determine the order in which to execute build steps, considering dependencies"
  (let ((visited (make-hash-table :test 'eq))
        (result '()))
    
    (labels ((visit (step-name)
               (unless (gethash step-name visited)
                 ;; Mark as visiting
                 (setf (gethash step-name visited) :visiting)
                 
                 ;; Visit dependencies first
                 (let ((step (get-build-step step-name)))
                   (dolist (dep (build-step-dependencies step))
                     (case (gethash dep visited)
                       (:visiting
                        (error 'configuration-error
                              :message (format nil "Circular dependency detected: ~A -> ~A" 
                                             step-name dep)
                              :key step-name))
                       (nil (visit dep))))
                   
                   ;; Add step to result and mark as visited
                   (push step-name result)
                   (setf (gethash step-name visited) :visited)))))
      
      ;; Visit all steps
      (dolist (step-name steps)
        (visit step-name)))
    
    ;; Return result in correct order
    (nreverse result)))

(defun run-build (context &optional step)
  "Run the build process"
  (let ((steps (if step
                  (list step)
                  (get-step-execution-order (list :prepare :get :chroot :conf :build)))))
    
    ;; Record build start time
    (setf (build-context-start-time context) (get-universal-time))
    
    ;; Log build start
    (log-message :info "Starting OneFileLinux build process")
    (when (build-context-verbose context)
      (log-message :info "Verbose mode enabled"))
    (when (build-context-dry-run context)
      (log-message :info "Dry run mode enabled~A" 
                 (if (build-context-dry-run-until context)
                     (format nil ", running until step: ~A" (build-context-dry-run-until context))
                     "")))
    
    ;; Execute steps in order
    (loop for step-name in steps
          for should-skip = (and (build-context-dry-run context)
                               (build-context-dry-run-until context)
                               (let ((step-pos (position step-name steps))
                                     (target-pos (position (build-context-dry-run-until context) steps)))
                                 (and target-pos step-pos (> step-pos target-pos))))
          do (cond 
               (should-skip
                (log-message :info "Skipping build step ~A (dry run)" step-name))
               (t 
                (let ((status (execute-build-step step-name context)))
                  (unless status
                    (log-message :error "Build failed at step: ~A" step-name)
                    (return-from run-build nil))
                  (when (and (build-context-dry-run context)
                           (build-context-dry-run-until context)
                           (eq step-name (build-context-dry-run-until context)))
                    (log-message :info "Dry run completed - reached target step: ~A" step-name)
                    (return-from run-build t))))))
    
    ;; Calculate and report build time
    (let* ((build-end-time (get-universal-time))
           (build-duration (- build-end-time (build-context-start-time context)))
           (minutes (floor build-duration 60))
           (seconds (mod build-duration 60)))
      
      (if (build-context-dry-run context)
          (log-message :success "Dry run completed successfully!")
          (log-message :success "Build completed successfully!"))
      (log-message :info "Total ~Atime: ~Am ~As" 
                 (if (build-context-dry-run context) "dry run " "build ")
                 minutes seconds)
      
      ;; Report individual step times
      (when (build-context-verbose context)
        (log-message :info "~A step timing:" 
                   (if (build-context-dry-run context) "Dry run" "Build"))
        (maphash (lambda (step-name duration)
                  (log-message :info "  ~A: ~As" step-name duration))
                (build-context-timing context)))
      
      t)))

;;; ===============================
;;; Command Line Parser
;;; ===============================

(defun print-usage ()
  "Display usage information"
  (format t "OneFileLinux Build System~%~%")
  (format t "Usage: build.lisp [options] [STEP]~%~%")
  (format t "Options:~%")
  (format t "  -h, --help          Display this help message~%")
  (format t "  -c, --clean-start   Clean before building~%")
  (format t "  -v, --verbose       Enable verbose output~%")
  (format t "  -s, --skip-prepare  Skip preparation step~%")
  (format t "  -C, --clean-end     Clean after building~%~%")
  (format t "Dry Run Options:~%")
  (format t "  --dry-run           Run in dry run mode (no actual build)~%")
  (format t "  --dry-run-until=STEP Dry run up to and including specified step~%")
  (format t "  --list-steps        List available build steps~%~%")
  (format t "Build Profiles:~%")
  (format t "  --minimal           Minimal profile (no ZFS, smallest size)~%")
  (format t "  --standard          Standard profile (default)~%")
  (format t "  --full              Full profile (all features)~%")
  (format t "  --profile=NAME      Custom profile by name~%")
  (format t "  --list-profiles     List available profiles~%~%")
  (format t "Package Groups:~%")
  (format t "  --package-groups=GROUP1,GROUP2,...  Specify package groups directly~%")
  (format t "  --include-group=NAME     Include a specific package group~%")
  (format t "  --exclude-group=NAME     Exclude a specific package group~%")
  (format t "  --list-package-groups    List all available package groups~%")
  (format t "  --extra-packages=PKG1,PKG2,...  Add extra individual packages~%~%")
  (format t "Build Steps:~%"))

(defun starts-with (prefix string)
  "Check if string starts with prefix"
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun parse-arguments (args)
  "Parse command line arguments and update configuration"
  (let* ((args (or args ()))  ;; Ensure args is not nil
         (separator-index (position "--" args :test #'string=))
         (regular-args (if separator-index 
                         (subseq args 0 separator-index) 
                         args))
         (passthrough-args (when separator-index 
                            (subseq args (1+ separator-index))))
         (step nil)
         (explicit-profile nil)
         (dry-run nil)
         (dry-run-until nil))
    
    ;; Check for profile args first
    (dolist (arg regular-args)
      (cond
        ((string= arg "--minimal")
         (apply-profile "minimal")
         (setf explicit-profile t))
        ((string= arg "--standard")
         (apply-profile "standard")
         (setf explicit-profile t))
        ((string= arg "--full")
         (apply-profile "full")
         (setf explicit-profile t))
        ((starts-with "--profile=" arg)
         (apply-profile (subseq arg 10))
         (setf explicit-profile t))))
    
    ;; Process regular arguments
    (dolist (arg regular-args)
      (cond
        ;; Help
        ((or (string= arg "-h") (string= arg "--help"))
         (print-usage)
         (return-from parse-arguments (values nil nil nil nil nil)))
        
        ;; Basic options
        ((or (string= arg "-c") (string= arg "--clean-start"))
         (set-config-value 'build-config 'clean-start t))
        ((or (string= arg "-v") (string= arg "--verbose"))
         (set-config-value 'build-config 'verbose t))
        ((or (string= arg "-C") (string= arg "--clean-end"))
         (set-config-value 'build-config 'clean-end t))
        
        ;; Dry run options
        ((string= arg "--dry-run")
         (setf dry-run t))
        ((starts-with "--dry-run-until=" arg)
         (let ((step-name (string-upcase (subseq arg 16))))
           (setf dry-run t)
           (setf dry-run-until (intern step-name "KEYWORD"))))
        ((string= arg "--list-steps")
         (list-steps-command)
         (return-from parse-arguments (values nil nil nil nil nil)))
        
        ;; Profile, already handled
        ((or (string= arg "--minimal") (string= arg "--standard") (string= arg "--full")
             (starts-with "--profile=" arg))
         t) ; Skip, already handled
        
        ;; Profile and package group listing
        ((string= arg "--list-profiles")
         (list-profiles-command)
         (return-from parse-arguments (values nil nil nil nil nil)))
        ((string= arg "--list-package-groups")
         (list-package-groups-command)
         (return-from parse-arguments (values nil nil nil nil nil)))
        
        ;; Feature flags
        ((string= arg "--with-zfs")
         (set-config-value 'feature-config 'include-zfs t))
        ((string= arg "--without-zfs")
         (set-config-value 'feature-config 'include-zfs nil))
        ((string= arg "--with-btrfs")
         (set-config-value 'feature-config 'include-btrfs t))
        ((string= arg "--without-btrfs")
         (set-config-value 'feature-config 'include-btrfs nil))
        ((string= arg "--with-recovery-tools")
         (set-config-value 'feature-config 'include-recovery-tools t))
        ((string= arg "--without-recovery-tools")
         (set-config-value 'feature-config 'include-recovery-tools nil))
        ((string= arg "--with-network-tools")
         (set-config-value 'feature-config 'include-network-tools t))
        ((string= arg "--without-network-tools")
         (set-config-value 'feature-config 'include-network-tools nil))
        ((string= arg "--with-crypto")
         (set-config-value 'feature-config 'include-crypto t))
        ((string= arg "--without-crypto")
         (set-config-value 'feature-config 'include-crypto nil))
        ((string= arg "--with-tui")
         (set-config-value 'feature-config 'include-tui t))
        ((string= arg "--without-tui")
         (set-config-value 'feature-config 'include-tui nil))
        
        ;; Package Groups
        ((starts-with "--package-groups=" arg)
         (let* ((groups-str (subseq arg 16))
                (group-names (mapcar (lambda (name)
                                      (intern (string-upcase (string-trim " " name)) "KEYWORD"))
                                    (split-string groups-str ","))))
           (set-config-value 'package-config 'package-groups group-names)))
        
        ;; Individual group inclusion/exclusion
        ((starts-with "--include-group=" arg)
         (let* ((group-name (string-upcase (string-trim " " (subseq arg 15))))
                (group-keyword (intern group-name "KEYWORD"))
                (current-groups (config-value 'package-config 'package-groups)))
           (set-config-value 'package-config 'package-groups 
                           (pushnew group-keyword current-groups))))
        
        ((starts-with "--exclude-group=" arg)
         (let* ((group-name (string-upcase (string-trim " " (subseq arg 15))))
                (group-keyword (intern group-name "KEYWORD"))
                (exclude-keyword (intern (concatenate 'string "NO-" group-name) "KEYWORD"))
                (current-groups (config-value 'package-config 'package-groups)))
           (set-config-value 'package-config 'package-groups
                           (pushnew exclude-keyword 
                                    (remove group-keyword current-groups)))))
        
        ;; Extra packages
        ((starts-with "--extra-packages=" arg)
         (let* ((packages-str (subseq arg 16))
                (package-list (split-string packages-str ",")))
           (set-config-value 'package-config 'extra-packages package-list)))
        
        ;; Build step
        ((member arg '("all" "prepare" "get" "chroot" "conf" "apkbuild" "build" "clean") 
                :test #'string=)
         (setf step (intern (string-upcase arg) "KEYWORD")))))
    
    ;; If dry-run-until is set without a step, use the last step in the process
    (when (and dry-run-until (not (member dry-run-until '(:prepare :get :chroot :conf :build :clean))))
      (log-message :warning "Unknown step for dry-run-until: ~A. Using :build instead." dry-run-until)
      (setf dry-run-until :build))
    
    ;; Return parsed values
    (values step passthrough-args explicit-profile dry-run dry-run-until)))

(defun list-profiles-command ()
  "Display available build profiles"
  (format t "Available build profiles:~%~%")
  (dolist (profile (list-profiles))
    (format t "  - ~A: ~A~%" 
           (build-profile-name profile)
           (build-profile-description profile))
    (format t "    Package Groups: ~A~%" 
           (build-profile-package-groups profile))
    
    ;; Extract key features for display
    (let ((features-list '()))
      (loop for (feature value) on (build-profile-features profile) by #'cddr
            when (and (symbolp feature) 
                     (string-prefix-p "INCLUDE-" (symbol-name feature))
                     value)
            do (push (subseq (symbol-name feature) 8) features-list))
      
      (when features-list
        (format t "    Features: ~{~A~^, ~}~%" features-list)))
    
    (format t "~%")))

(defun list-steps-command ()
  "Display available build steps"
  (format t "Available build steps:~%~%")
  (dolist (step (list-build-steps))
    (format t "  - ~A: ~A~%" 
           (build-step-name step)
           (build-step-description step))
    
    ;; Show dependencies if any
    (when (build-step-dependencies step)
      (format t "    Dependencies: ~{~A~^, ~}~%" 
             (build-step-dependencies step)))
    
    (format t "~%")))

(defun list-package-groups-command ()
  "Display available package groups"
  (format t "Available package groups:~%~%")
  (dolist (group (list-package-groups))
    (format t "  - ~A: ~A~%" 
           (package-group-name group)
           (package-group-description group))
    
    ;; Show if it's always included
    (when (package-group-always-include group)
      (format t "    Always included: Yes~%"))
    
    ;; Show contents if it has direct packages
    (when (package-group-packages group)
      (format t "    Packages: ~{~A~^, ~}~%" 
             (package-group-packages group)))
    
    ;; Show dependencies if any
    (when (package-group-dependencies group)
      (format t "    Dependencies: ~{~A~^, ~}~%" 
             (package-group-dependencies group)))
    
    ;; Show conflicts if any
    (when (package-group-conflicts group)
      (format t "    Conflicts with: ~{~A~^, ~}~%" 
             (package-group-conflicts group)))
    
    (format t "~%")))

;;; ===============================
;;; Main Entry Point
;;; ===============================

(defun main (args)
  "Main entry point for the build system"
  ;; Handle nil args case explicitly to avoid errors
  (setf args (or args ()))
  (format t "Build main called with args: ~S~%" args)
  
  ;; Parse arguments with error handling
  (handler-case
      (multiple-value-bind (step passthrough-args explicit-profile dry-run dry-run-until)
          (parse-arguments args)
    
    (when (null step)
      (when explicit-profile
        (return-from main 0))
      (setf step :all))
    
    ;; Create build context
    (let* ((working-dir (uiop:getcwd))
           (output-dir (ensure-directory 
                        (path-join working-dir 
                                  (config-value 'system-config 'artifact-paths :output))))
           (context (make-build-context
                    :config *config*
                    :working-dir working-dir
                    :output-dir output-dir
                    :args passthrough-args
                    :verbose (config-value 'build-config 'verbose nil)
                    :dry-run dry-run
                    :dry-run-until dry-run-until)))
      
      ;; Print build banner
      (print-build-banner)
      
      ;; Run the build
      (if (eq step :all)
          (run-build context)
          (execute-build-step step context))))
    (error (e)
      (format t "ERROR in build main: ~A~%" e)
      (format t "Args were: ~S~%" args)
      (return-from main 1)))
  
  ;; Return success
  0)

(defun print-build-banner ()
  "Print the build system banner"
  (format t "~%      ____________  ~%")
  (format t "    /|------------| ~%")
  (format t "   /_|  .---.     | ~%")
  (format t "  |    /     \\    | ~%")
  (format t "  |    \\.6-6./    | ~%")
  (format t "  |    /`\\_/`\\    | ~%")
  (format t "  |   //  _  \\\\   | ~%")
  (format t "  |  | \\     / |  | ~%")
  (format t "  | /`\\_`>  <_/`\\ | ~%")
  (format t "  | \\__/'---'\\__/ | ~%")
  (format t "  |_______________| ~%")
  (format t "   OneFileLinux Builder  ~%")
  (format t "----------------------------------------------------~%~%"))