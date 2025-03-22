;;;; OneFileLinux Build Step: Get
;;;; Download and extract source files for the build process

(defpackage :onefilelinux.build.get
  (:use :cl :onefilelinux.core :onefilelinux.config :onefilelinux.build)
  (:export #:make-get-step
           #:register-get-step))

(in-package :onefilelinux.build.get)

;;; ===============================
;;; Get Step Implementation
;;; ===============================

(defclass get-step (build-step)
  ()
  (:default-initargs 
   :name :get
   :description "Download and extract source files"
   :dependencies '(:prepare))
  (:documentation "Build step for downloading and extracting source files"))

(defmethod prepare ((step get-step) context)
  (log-message :info "Preparing to download sources")
  
  ;; Create download directories
  (let* ((artifact-paths (config-value 'system-config 'artifact-paths))
         (download-dir (path-join (build-context-working-dir context)
                               (getf artifact-paths :downloads)))
         (extract-dir (path-join (build-context-working-dir context)
                               (getf artifact-paths :extracted))))
    
    (ensure-directory download-dir)
    (ensure-directory extract-dir))
  
  t)

(defmethod execute ((step get-step) context)
  (log-message :info "Downloading source files")
  
  ;; Download Alpine Linux
  (download-alpine context)
  
  ;; Download Linux kernel
  (download-kernel context)
  
  t)

(defmethod cleanup ((step get-step) context status)
  (log-message :info "Cleaning up after downloads")
  
  ;; Nothing to clean up
  t)

;;; ===============================
;;; Helper Functions
;;; ===============================

(defun download-alpine (context)
  "Download Alpine Linux minirootfs"
  (log-message :info "Downloading Alpine Linux minirootfs")
  
  (let* ((alpine-version (config-value 'system-config 'alpine-version))
         (minirootfs-file (config-value 'system-config 'alpine-minirootfs-file))
         (download-urls (config-value 'system-config 'download-urls))
         (alpine-url (format nil (getf download-urls :alpine) 
                           alpine-version minirootfs-file))
         (artifact-paths (config-value 'system-config 'artifact-paths))
         (download-dir (path-join (build-context-working-dir context)
                               (getf artifact-paths :downloads)))
         (download-path (path-join download-dir minirootfs-file)))
    
    ;; Check if already downloaded
    (if (file-exists-p download-path)
        (log-message :info "Alpine Linux already downloaded: ~A" download-path)
        
        ;; Download file
        (progn
          (log-message :info "Downloading from ~A" alpine-url)
          (run-command (format nil "wget -q --show-progress -O ~A ~A" 
                             download-path alpine-url))
          (log-message :success "Downloaded Alpine Linux minirootfs")))
    
    ;; Extract if needed
    (extract-alpine context download-path)))

(defun extract-alpine (context download-path)
  "Extract Alpine Linux minirootfs"
  (log-message :info "Extracting Alpine Linux minirootfs")
  
  (let* ((artifact-paths (config-value 'system-config 'artifact-paths))
         (rootfs-dir (path-join (build-context-working-dir context)
                              "alpine-minirootfs")))
    
    ;; Check if already extracted
    (if (and (directory-exists-p rootfs-dir)
             (file-exists-p (path-join rootfs-dir "etc" "alpine-release")))
        (log-message :info "Alpine Linux already extracted: ~A" rootfs-dir)
        
        ;; Extract tar.gz
        (progn
          (ensure-directory rootfs-dir)
          (run-command (format nil "tar -xzf ~A -C ~A" download-path rootfs-dir))
          (log-message :success "Extracted Alpine Linux minirootfs")))))

(defun download-kernel (context)
  "Download Linux kernel source"
  (log-message :info "Downloading Linux kernel")
  
  (let* ((kernel-version (config-value 'system-config 'kernel-version))
         (linux-version-string (config-value 'system-config 'linux-version-string))
         (download-urls (config-value 'system-config 'download-urls))
         (kernel-url (format nil (getf download-urls :kernel) linux-version-string))
         (artifact-paths (config-value 'system-config 'artifact-paths))
         (download-dir (path-join (build-context-working-dir context)
                               (getf artifact-paths :downloads)))
         (download-path (path-join download-dir 
                                 (format nil "~A.tar.xz" linux-version-string))))
    
    ;; Check if already downloaded
    (if (file-exists-p download-path)
        (log-message :info "Linux kernel already downloaded: ~A" download-path)
        
        ;; Download file
        (progn
          (log-message :info "Downloading from ~A" kernel-url)
          (run-command (format nil "wget -q --show-progress -O ~A ~A" 
                             download-path kernel-url))
          (log-message :success "Downloaded Linux kernel")))
    
    ;; Extract if needed
    (extract-kernel context download-path)))

(defun extract-kernel (context download-path)
  "Extract Linux kernel source"
  (log-message :info "Extracting Linux kernel")
  
  (let* ((artifact-paths (config-value 'system-config 'artifact-paths))
         (linux-version-string (config-value 'system-config 'linux-version-string))
         (kernel-dir (path-join (build-context-working-dir context) "linux")))
    
    ;; Check if already extracted
    (if (and (directory-exists-p kernel-dir)
             (file-exists-p (path-join kernel-dir "Makefile")))
        (log-message :info "Linux kernel already extracted: ~A" kernel-dir)
        
        ;; Extract tar.xz
        (progn
          (ensure-directory kernel-dir)
          (run-command (format nil "tar -xJf ~A -C ~A --strip-components=1" 
                             download-path kernel-dir))
          (log-message :success "Extracted Linux kernel")))))

;;; ===============================
;;; Step Registration
;;; ===============================

(defun make-get-step ()
  "Create a new get step instance"
  (make-instance 'get-step))

(defun register-get-step ()
  "Register the get step with the build system"
  (register-build-step (make-get-step)))

;; Register the step when this file is loaded
(register-get-step)