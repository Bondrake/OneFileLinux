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
  ((name :initform "chroot-and-install"
         :allocation :class
         :reader step-name)
   (description :initform "Sets up chroot environment and installs packages"
                :allocation :class
                :reader step-description))
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
    (let ((rootfs-dir (path-join working-dir "rootfs"))
          (arch (config-value config :system :arch "x86_64"))
          (profile (config-value config :build :profile "minimal")))
      
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

(defmethod cleanup ((step chrootandinstall-step) (context build-context))
  "Clean up after chroot and package installation."
  (with-slots (working-dir config) context
    (let ((rootfs-dir (path-join working-dir "rootfs"))
          (preserve-work (config-value config :build :preserve-work nil)))
      
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
  (let ((repositories-file (path-join rootfs-dir "etc/apk/repositories"))
        (alpine-version (config-value (context-config *build-context*) 
                                      :system :alpine-version "3.19.1")))
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
    (run-command "apk" '("update") :capture-output nil)))

(defun install-packages (rootfs-dir profile context)
  "Install required packages based on the build profile."
  (with-slots (config) context
    (let ((packages (resolve-required-packages profile config)))
      (log-message :debug "Installing packages: ~{~A~^, ~}" packages)
      
      (with-chroot rootfs-dir
        (apply #'run-command 
               "apk" 
               (append '("add" "--no-cache") packages)
               '(:capture-output nil))))))

(defun resolve-required-packages (profile config)
  "Resolve the list of packages required for the given profile."
  (let ((base-packages (config-value config :packages :base 
                                    '("alpine-base" "linux-lts" "alpine-baselayout")))
        (profile-packages (config-value config :packages profile nil))
        (feature-packages '()))
    
    ;; Gather packages from enabled features
    (let ((enabled-features (config-value config :features :enabled nil)))
      (dolist (feature enabled-features)
        (let ((feature-pkgs (config-value config :packages feature nil)))
          (when feature-pkgs
            (setf feature-packages (append feature-packages feature-pkgs))))))
    
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
    (run-command "rc-update" '("add" "devfs" "sysinit") :capture-output nil)
    (run-command "rc-update" '("add" "dmesg" "sysinit") :capture-output nil)
    (run-command "rc-update" '("add" "mdev" "sysinit") :capture-output nil)))

(defun clean-apk-cache (rootfs-dir)
  "Clean the APK cache in the chroot environment."
  (with-chroot rootfs-dir
    (run-command "rm" '("-rf" "/var/cache/apk/*") :capture-output nil)))

(defun safe-unmount-special (rootfs-dir)
  "Safely unmount any special filesystems that might be mounted."
  (dolist (special '("dev" "proc" "sys"))
    (let ((mount-point (path-join rootfs-dir special)))
      (when (is-mounted-p mount-point)
        (run-command "umount" (list "-f" mount-point) :capture-output nil)))))

(defun is-mounted-p (path)
  "Check if a path is currently mounted."
  (let ((result (run-command-status "mountpoint" (list "-q" path))))
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