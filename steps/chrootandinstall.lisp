;;;; OneFileLinux Build Step: Chroot and Install
;;;; Sets up chroot environment and installs packages
(defpackage :onefilelinux.build.chrootandinstall
  (:use :cl :onefilelinux.core :onefilelinux.config :onefilelinux.build)
  (:export #:make-chrootandinstall-step
           #:register-chrootandinstall-step))

(in-package :onefilelinux.build.chrootandinstall)

;;; ----------------------------------------------------
;;; Build Step Implementation - Chroot and Install
;;; ----------------------------------------------------

(defclass chrootandinstall-step (build-step)
  ((name :initform :chroot
         :allocation :class
         :reader build-step-name)
   (description :initform "Sets up chroot environment and installs packages"
                :allocation :class
                :reader build-step-description))
  (:documentation "Step to set up chroot environment and install packages."))

(defmethod prepare ((step chrootandinstall-step) (context build-context))
  "Prepare for chroot and package installation."
  (with-slots (working-dir) context
    (let ((rootfs-dir (path-join working-dir "rootfs")))
      ;; Validate required directories
      (unless (directory-exists-p rootfs-dir)
        (error 'onefilelinux-error 
               :message "Rootfs directory not found. Did the 'get' step complete successfully?"))
      
      ;; Create necessary directories
      (ensure-directories-exist (path-join rootfs-dir "etc"))
      (ensure-directories-exist (path-join rootfs-dir "etc/apk"))
      
      ;; Log preparation status
      (log-message :info "Prepared environment for chroot and package installation")))
  t)

(defmethod execute ((step chrootandinstall-step) (context build-context))
  "Execute chroot setup and package installation."
  (with-slots (working-dir config) context
    (let* ((rootfs-dir (path-join working-dir "rootfs"))
           ;; Fix config-value calls to use proper slot access
           (system-config (gethash 'system-config config))
           (build-config (gethash 'build-config config))
           (arch (if (slot-exists-p system-config 'arch)
                    (slot-value system-config 'arch)
                    "x86_64"))
           (profile (if (slot-exists-p build-config 'profile)
                       (slot-value build-config 'profile)
                       "minimal")))
      
      ;; 1. Configure DNS for chroot
      (log-message :info "Setting up DNS for chroot...")
      (setup-chroot-dns rootfs-dir)
      
      ;; 2. Configure APK repositories
      (log-message :info "Configuring APK repositories...")
      (setup-apk-repositories rootfs-dir arch)
      
      ;; 3. Update APK database
      (log-message :info "Updating APK database...")
      (update-apk-database rootfs-dir)
      
      ;; 4. Install required packages
      (log-message :info "Installing packages for profile: ~A..." profile)
      (install-packages rootfs-dir profile context)
      
      ;; 5. Perform post-installation setup
      (log-message :info "Performing post-installation setup...")
      (post-install-setup rootfs-dir)
      
      (log-message :info "Successfully completed chroot and package installation")))
  t)

(defmethod cleanup ((step chrootandinstall-step) (context build-context) &optional status)
  "Clean up after chroot and package installation."
  (with-slots (working-dir config) context
    (let* ((rootfs-dir (path-join working-dir "rootfs"))
           ;; Fix config-value calls to use proper slot access
           (build-config (gethash 'build-config config))
           (preserve-work (if (slot-exists-p build-config 'preserve-work)
                             (slot-value build-config 'preserve-work)
                             nil)))
      
      ;; Clean APK cache if not preserving work
      (unless preserve-work
        (log-message :info "Cleaning APK cache...")
        (clean-apk-cache rootfs-dir))
      
      ;; Unmount any potential mounts
      (safe-unmount-special rootfs-dir)))
  t)

;;; ----------------------------------------------------
;;; Helper Functions
;;; ----------------------------------------------------

(defun setup-chroot-dns (rootfs-dir)
  "Set up DNS in the chroot environment."
  (let ((resolv-conf (path-join rootfs-dir "etc/resolv.conf")))
    (with-open-file (out resolv-conf :direction :output 
                                     :if-exists :supersede 
                                     :if-does-not-exist :create)
      (format out "nameserver 8.8.8.8~%")
      (format out "nameserver 8.8.4.4~%"))
    (log-message :debug "Created resolv.conf at ~A" resolv-conf)))

(defun setup-apk-repositories (rootfs-dir arch)
  "Set up APK repositories configuration."
  (let* ((repositories-file (path-join rootfs-dir "etc/apk/repositories"))
         ;; Get alpine version safely
         (alpine-version (if (boundp '*build-context*)
                            (let* ((config (build-context-config *build-context*))
                                  (system-config (gethash 'system-config config)))
                              (if (and system-config (slot-exists-p system-config 'alpine-version))
                                  (slot-value system-config 'alpine-version)
                                  "3.19.1"))
                            "3.19.1")))
    (with-open-file (out repositories-file :direction :output 
                                          :if-exists :supersede 
                                          :if-does-not-exist :create)
      (format out "https://dl-cdn.alpinelinux.org/alpine/v~A/main~%" 
              (subseq alpine-version 0 4))
      (format out "https://dl-cdn.alpinelinux.org/alpine/v~A/community~%" 
              (subseq alpine-version 0 4)))
    (log-message :debug "Created repositories file at ~A" repositories-file)))

(defun update-apk-database (rootfs-dir)
  "Update the APK database within the chroot."
  (with-chroot rootfs-dir
    (run-command "apk" '("update"))))

(defun install-packages (rootfs-dir profile context)
  "Install required packages based on the build profile."
  (with-slots (config) context
    (let ((packages (resolve-required-packages profile config)))
      (log-message :debug "Installing packages: ~{~A~^, ~}" packages)
      
      (with-chroot rootfs-dir
        (run-command 
         "apk" 
         (append '("add" "--no-cache") packages))))))

(defun resolve-required-packages (profile config)
  "Resolve the list of packages required for the given profile."
  (let* ((package-config (gethash 'package-config config))
         (default-packages '("alpine-base" "linux-lts" "alpine-baselayout"))
         (base-packages 
           (if (and package-config (slot-exists-p package-config 'base-packages))
               (slot-value package-config 'base-packages)
               default-packages))
         (profile-packages '())
         (feature-packages '()))
    
    ;; Try to get profile packages if they exist
    (when (and package-config 
              (slot-exists-p package-config 'profile-packages)
              (slot-value package-config 'profile-packages))
      (let ((profiles (slot-value package-config 'profile-packages)))
        (when (and (listp profiles) (getf profiles (intern (string-upcase profile) :keyword)))
          (setf profile-packages (getf profiles (intern (string-upcase profile) :keyword))))))
    
    ;; Try to get feature packages
    (let ((feature-config (gethash 'feature-config config)))
      (when (and feature-config (slot-exists-p feature-config 'enabled-features))
        (let ((enabled-features (slot-value feature-config 'enabled-features)))
          (dolist (feature enabled-features)
            (when (and package-config 
                      (slot-exists-p package-config 'feature-packages)
                      (slot-value package-config 'feature-packages))
              (let* ((feature-pkgs-table (slot-value package-config 'feature-packages))
                    (feature-pkgs (getf feature-pkgs-table feature)))
                (when feature-pkgs
                  (setf feature-packages (append feature-packages feature-pkgs)))))))))
    
    ;; Combine all package sets, removing duplicates
    (remove-duplicates 
     (append base-packages profile-packages feature-packages)
     :test #'string=)))

(defun post-install-setup (rootfs-dir)
  "Perform post-installation setup."
  ;; Ensure critical directories exist
  (ensure-directories-exist (path-join rootfs-dir "etc/runlevels/default"))
  (ensure-directories-exist (path-join rootfs-dir "root"))
  
  ;; Configure default runlevel services
  (with-chroot rootfs-dir
    (run-command "rc-update" '("add" "devfs" "sysinit"))
    (run-command "rc-update" '("add" "dmesg" "sysinit"))
    (run-command "rc-update" '("add" "mdev" "sysinit"))))

(defun clean-apk-cache (rootfs-dir)
  "Clean the APK cache in the chroot environment."
  (with-chroot rootfs-dir
    (run-command "rm" '("-rf" "/var/cache/apk/*"))))

(defun safe-unmount-special (rootfs-dir)
  "Safely unmount any special filesystems that might be mounted."
  (dolist (special '("dev" "proc" "sys"))
    (let ((mount-point (path-join rootfs-dir special)))
      (when (is-mounted-p mount-point)
        (run-command "umount" (list "-f" mount-point))))))

(defun is-mounted-p (path)
  "Check if a path is currently mounted."
  (let ((result (multiple-value-bind (output exit-code)
                    (run-command "mountpoint" (list "-q" path) :ignore-error t)
                  (declare (ignore output))
                  exit-code)))
    (= result 0)))

;;; ----------------------------------------------------
;;; Macros and Utilities
;;; ----------------------------------------------------

(defmacro with-chroot ((rootfs-dir) &body body)
  "Execute body in a chroot context."
  `(let ((original-dir (getcwd)))
     (unwind-protect
          (progn
            ;; Mount special filesystems
            (run-command "mount" (list "--bind" "/dev" (path-join ,rootfs-dir "dev")))
            (run-command "mount" (list "-t" "proc" "none" (path-join ,rootfs-dir "proc")))
            (run-command "mount" (list "-t" "sysfs" "none" (path-join ,rootfs-dir "sys")))
            
            ;; Change to rootfs directory and execute chrooted commands
            (chdir ,rootfs-dir)
            ,@body)
       
       ;; Cleanup and restore original directory
       (chdir original-dir)
       (safe-unmount-special ,rootfs-dir))))

;;; ----------------------------------------------------
;;; Build Step Registration
;;; ----------------------------------------------------

(defun make-chrootandinstall-step ()
  "Create a new instance of the chrootandinstall-step."
  (make-instance 'chrootandinstall-step))

(defun register-chrootandinstall-step ()
  "Register the chrootandinstall step with the build system."
  (register-build-step (make-chrootandinstall-step)))

;; Auto-register when the package is loaded
(eval-when (:load-toplevel :execute)
  (register-chrootandinstall-step))