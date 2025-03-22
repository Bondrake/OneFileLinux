;;;; OneFileLinux GitHub Actions Integration
;;;; Utilities for integrating with GitHub Actions CI/CD
(defpackage :onefilelinux.github.actions
  (:use :cl :onefilelinux.core :onefilelinux.config)
  (:export #:setup-github-environment
           #:start-github-group
           #:end-github-group
           #:set-github-output
           #:set-github-annotation
           #:setup-github-build-matrix
           #:report-build-result
           #:with-github-step))

(in-package :onefilelinux.github.actions)

;;; ----------------------------------------------------
;;; GitHub Actions Environment
;;; ----------------------------------------------------

(defclass github-environment ()
  ((enabled :initarg :enabled
            :accessor github-enabled
            :initform nil
            :documentation "Whether GitHub Actions integration is enabled")
   (workflow :initarg :workflow
             :accessor github-workflow
             :initform nil
             :documentation "Current GitHub workflow name")
   (job :initarg :job
        :accessor github-job
        :initform nil
        :documentation "Current GitHub job name")
   (step :initarg :step
         :accessor github-step
         :initform nil
         :documentation "Current GitHub step name")
   (actor :initarg :actor
          :accessor github-actor
          :initform nil
          :documentation "GitHub user who triggered the workflow")
   (repository :initarg :repository
               :accessor github-repository
               :initform nil
               :documentation "GitHub repository name")
   (run-id :initarg :run-id
           :accessor github-run-id
           :initform nil
           :documentation "GitHub Actions run ID")
   (output-file :initarg :output-file
                :accessor github-output-file
                :initform nil
                :documentation "Path to GitHub output file"))
  (:documentation "Information about the GitHub Actions environment."))

(defvar *github-env* nil
  "The current GitHub Actions environment.")

(defun setup-github-environment ()
  "Set up GitHub Actions integration by detecting the environment."
  (let ((github-actions (uiop:getenv "GITHUB_ACTIONS")))
    (if (and github-actions (string-equal github-actions "true"))
        (let ((env (make-instance 'github-environment
                                 :enabled t
                                 :workflow (uiop:getenv "GITHUB_WORKFLOW")
                                 :job (uiop:getenv "GITHUB_JOB")
                                 :step nil  ; Set dynamically during steps
                                 :actor (uiop:getenv "GITHUB_ACTOR")
                                 :repository (uiop:getenv "GITHUB_REPOSITORY")
                                 :run-id (uiop:getenv "GITHUB_RUN_ID")
                                 :output-file (uiop:getenv "GITHUB_OUTPUT"))))
          (setf *github-env* env)
          (log-message :info "GitHub Actions integration enabled")
          (log-message :debug "GitHub Workflow: ~A, Job: ~A, Run ID: ~A"
                      (github-workflow env)
                      (github-job env)
                      (github-run-id env))
          t)
        (progn
          (setf *github-env* nil)
          (log-message :debug "GitHub Actions integration not detected")
          nil))))

(defun github-actions-enabled-p ()
  "Check if GitHub Actions integration is enabled."
  (and *github-env* (github-enabled *github-env*)))

;;; ----------------------------------------------------
;;; GitHub Actions Commands
;;; ----------------------------------------------------

(defun start-github-group (title)
  "Start a collapsible group in GitHub Actions logs."
  (when (github-actions-enabled-p)
    (format t "::group::~A~%" title)
    (log-message :debug "Started GitHub group: ~A" title)))

(defun end-github-group ()
  "End a collapsible group in GitHub Actions logs."
  (when (github-actions-enabled-p)
    (format t "::endgroup::~%")
    (log-message :debug "Ended GitHub group")))

(defun set-github-output (name value)
  "Set an output parameter for the GitHub Actions step."
  (when (github-actions-enabled-p)
    (let ((output-file (github-output-file *github-env*)))
      (when output-file
        (with-open-file (out output-file :direction :output
                                       :if-exists :append
                                       :if-does-not-exist :create)
          (format out "~A=~A~%" name value))
        (log-message :debug "Set GitHub output: ~A=~A" name value)))))

(defun set-github-annotation (type message &key file line title)
  "Set a GitHub Actions annotation (warning, error, or notice)."
  (when (github-actions-enabled-p)
    (let ((annotation (format nil "::~A" type)))
      (when file
        (setf annotation (format nil "~A file=~A" annotation file)))
      (when line
        (setf annotation (format nil "~A,line=~A" annotation line)))
      (when title
        (setf annotation (format nil "~A,title=~A" annotation title)))
      
      (format t "~A::~A~%" annotation message)
      (log-message :debug "Set GitHub annotation: ~A - ~A" type message))))

(defmacro with-github-step ((step-name) &body body)
  "Execute body as a GitHub Actions step."
  `(progn
     (when (github-actions-enabled-p)
       (setf (github-step *github-env*) ,step-name)
       (start-github-group ,step-name))
     
     (unwind-protect
          (handler-case
              (progn ,@body)
            (onefilelinux-error (err)
              (when (github-actions-enabled-p)
                (set-github-annotation "error" (error-message err)))
              (error err))
            (error (err)
              (when (github-actions-enabled-p)
                (set-github-annotation "error" (format nil "~A" err)))
              (error err)))
       
       (when (github-actions-enabled-p)
         (end-github-group)))))

;;; ----------------------------------------------------
;;; GitHub Actions Build Matrix
;;; ----------------------------------------------------

(defun setup-github-build-matrix (config)
  "Configure a build based on GitHub Actions matrix parameters."
  (when (github-actions-enabled-p)
    (log-message :info "Configuring build from GitHub matrix parameters")
    
    ;; Get matrix parameters from environment variables
    (let ((profile (uiop:getenv "MATRIX_PROFILE"))
          (arch (uiop:getenv "MATRIX_ARCH"))
          (features (uiop:getenv "MATRIX_FEATURES")))
      
      ;; Apply matrix parameters to configuration
      (when profile
        (setf (config-value config :build :profile) profile)
        (log-message :debug "Set build profile from matrix: ~A" profile))
      
      (when arch
        (setf (config-value config :system :arch) arch)
        (log-message :debug "Set system architecture from matrix: ~A" arch))
      
      (when features
        (let ((feature-list (split-string features ",")))
          (setf (config-value config :features :enabled) feature-list)
          (log-message :debug "Set enabled features from matrix: ~{~A~^, ~}" feature-list))))))

;;; ----------------------------------------------------
;;; GitHub Actions Build Reporting
;;; ----------------------------------------------------

(defun report-build-result (success-p output-file build-time &optional error-message)
  "Report build results to GitHub Actions."
  (when (github-actions-enabled-p)
    (log-message :info "Reporting build results to GitHub Actions")
    
    ;; Set build status output
    (set-github-output "build_status" (if success-p "success" "failure"))
    
    ;; Set build output file
    (when output-file
      (set-github-output "output_file" output-file))
    
    ;; Set build time
    (set-github-output "build_time" (format nil "~D" build-time))
    
    ;; Report error if build failed
    (unless success-p
      (set-github-annotation "error" 
                            (or error-message "Build failed")
                            :title "Build Failure"))))