;;;; OneFileLinux APK Builder
;;;; Build system for creating custom Alpine Package Manager (APK) packages
(defpackage :onefilelinux.package.apk-builder
  (:use :cl :onefilelinux.core :onefilelinux.config)
  (:export #:build-apk-package
           #:build-package-repository
           #:install-custom-packages
           #:clear-package-build-cache))

(in-package :onefilelinux.package.apk-builder)

;;; ----------------------------------------------------
;;; Package Builder Core
;;; ----------------------------------------------------

(defclass apk-builder ()
  ((build-root :initarg :build-root
               :accessor builder-build-root
               :documentation "Root directory for APK building")
   (sources-dir :initarg :sources-dir
                :accessor builder-sources-dir
                :documentation "Directory for package source files")
   (output-dir :initarg :output-dir
               :accessor builder-output-dir
               :documentation "Directory for built APK packages")
   (keys-dir :initarg :keys-dir
             :accessor builder-keys-dir
             :documentation "Directory for APK signing keys")
   (config :initarg :config
           :accessor builder-config
           :documentation "Configuration context"))
  (:documentation "APK package builder with configuration context."))

(defmethod initialize-instance :after ((builder apk-builder) &key)
  "Initialize the APK builder with proper directory structure."
  ;; Create required directories
  (let ((build-root (builder-build-root builder)))
    (ensure-directories-exist (path-join build-root "packages"))
    (ensure-directories-exist (path-join build-root "sources"))
    (ensure-directories-exist (path-join build-root "keys"))
    (ensure-directories-exist (path-join build-root "repo"))))

;;; ----------------------------------------------------
;;; Public Interface
;;; ----------------------------------------------------

(defun build-apk-package (package-name &key 
                                      (version "1.0.0")
                                      (description "Custom OneFileLinux package")
                                      (depends '())
                                      (build-root *default-build-root*)
                                      (config *default-config*))
  "Build an APK package with the given specifications."
  (log-message :info "Building APK package: ~A version ~A" package-name version)
  
  ;; Create APK builder instance
  (let* ((builder (make-instance 'apk-builder
                                :build-root build-root
                                :sources-dir (path-join build-root "sources")
                                :output-dir (path-join build-root "packages")
                                :keys-dir (path-join build-root "keys")
                                :config config))
         ;; Source and build directories for this package
         (pkg-source-dir (path-join (builder-sources-dir builder) package-name))
         (pkg-build-dir (path-join (builder-build-root builder) "build" package-name)))
    
    ;; Ensure package source directory exists
    (unless (directory-exists-p pkg-source-dir)
      (error 'onefilelinux-error
             :message (format nil "Package source directory not found: ~A" pkg-source-dir)))
    
    ;; Create/clean build directory
    (when (directory-exists-p pkg-build-dir)
      (delete-directory-recursively pkg-build-dir))
    (ensure-directories-exist pkg-build-dir)
    
    ;; Initialize APK build environment
    (initialize-apk-build-environment builder)
    
    ;; Create APKBUILD file
    (create-apkbuild-file builder package-name pkg-build-dir
                         :version version
                         :description description
                         :depends depends)
    
    ;; Copy source files
    (copy-package-sources builder package-name pkg-source-dir pkg-build-dir)
    
    ;; Build the package
    (build-package builder package-name pkg-build-dir)
    
    ;; Return path to built package
    (let ((package-path (path-join (builder-output-dir builder)
                                  (format nil "~A-~A.apk" package-name version))))
      (if (file-exists-p package-path)
          (progn
            (log-message :info "Successfully built APK package: ~A" package-path)
            package-path)
          (progn
            (log-message :error "Failed to build APK package: ~A" package-name)
            nil)))))

(defun build-package-repository (packages &key 
                                        (build-root *default-build-root*)
                                        (config *default-config*))
  "Build an APK repository from a list of packages."
  (log-message :info "Building APK repository with ~D packages" (length packages))
  
  ;; Create APK builder instance
  (let* ((builder (make-instance 'apk-builder
                                :build-root build-root
                                :sources-dir (path-join build-root "sources")
                                :output-dir (path-join build-root "packages")
                                :keys-dir (path-join build-root "keys")
                                :config config))
         (repo-dir (path-join (builder-build-root builder) "repo")))
    
    ;; Build all packages
    (let ((built-packages '()))
      (dolist (package packages)
        (let ((pkg-name (if (listp package) (first package) package))
              (pkg-version (if (listp package) (second package) "1.0.0"))
              (pkg-desc (if (listp package) (third package) "Custom OneFileLinux package"))
              (pkg-deps (if (listp package) (fourth package) '())))
          
          (let ((pkg-path (build-apk-package pkg-name
                                           :version pkg-version
                                           :description pkg-desc
                                           :depends pkg-deps
                                           :build-root build-root
                                           :config config)))
            (when pkg-path
              (push pkg-path built-packages)))))
      
      ;; Create repository index
      (when built-packages
        (create-repository-index builder repo-dir built-packages)
        repo-dir))))

(defun install-custom-packages (rootfs-dir repo-dir &key
                                         (packages '())
                                         (config *default-config*))
  "Install custom packages into a rootfs from a repository."
  (log-message :info "Installing custom packages into rootfs: ~A" rootfs-dir)
  
  ;; Ensure directories exist
  (ensure-directories-exist (path-join rootfs-dir "etc/apk"))
  
  ;; Add custom repository to APK configuration
  (let ((repositories-file (path-join rootfs-dir "etc/apk/repositories")))
    (with-open-file (out repositories-file :direction :output
                                         :if-exists :append
                                         :if-does-not-exist :create)
      (format out "~%# OneFileLinux custom repository~%")
      (format out "file://~A~%" repo-dir)))
  
  ;; Update APK database in chroot
  (with-chroot rootfs-dir
    (run-command "apk update" :capture-output nil))
  
  ;; Install packages
  (when packages
    (log-message :debug "Installing packages: ~{~A~^, ~}" packages)
    (with-chroot rootfs-dir
      (run-command 
       (format nil "apk add --allow-untrusted ~{~A~^ ~}" packages)
       :capture-output nil)))
  
  t)

(defun clear-package-build-cache (&key (build-root *default-build-root*))
  "Clear package build cache directories."
  (log-message :info "Clearing package build cache")
  
  (let ((build-dir (path-join build-root "build")))
    (when (directory-exists-p build-dir)
      (delete-directory-recursively build-dir)
      (log-message :debug "Removed build directory: ~A" build-dir)))
  
  t)

;;; ----------------------------------------------------
;;; Helper Functions
;;; ----------------------------------------------------

(defun initialize-apk-build-environment (builder)
  "Initialize the APK build environment with necessary tools and keys."
  (let ((keys-dir (builder-keys-dir builder)))
    ;; Generate APK signing keys if they don't exist
    (unless (and (file-exists-p (path-join keys-dir "private.rsa"))
                (file-exists-p (path-join keys-dir "public.rsa")))
      (log-message :debug "Generating APK signing keys")
      (ensure-directories-exist keys-dir)
      (run-command "openssl" 
                  `("genrsa" "-out" ,(path-join keys-dir "private.rsa") "2048"))
      (run-command "openssl" 
                  `("rsa" "-in" ,(path-join keys-dir "private.rsa") 
                    "-pubout" "-out" ,(path-join keys-dir "public.rsa"))))))

(defun create-apkbuild-file (builder package-name build-dir &key
                                    (version "1.0.0")
                                    (description "Custom OneFileLinux package")
                                    (depends '()))
  "Create an APKBUILD file for package building."
  (log-message :debug "Creating APKBUILD file for package: ~A" package-name)
  
  (let ((apkbuild-path (path-join build-dir "APKBUILD")))
    (with-open-file (out apkbuild-path :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (format out "# Generated APKBUILD for OneFileLinux custom package~%")
      (format out "# Package: ~A~%~%" package-name)
      
      ;; Package metadata
      (format out "pkgname=~A~%" package-name)
      (format out "pkgver=~A~%" version)
      (format out "pkgrel=0~%")
      (format out "pkgdesc=\"~A\"~%" description)
      (format out "url=\"https://github.com/onefilelinux/onefilelinux\"~%")
      (format out "arch=\"all\"~%")
      (format out "license=\"MIT\"~%")
      (format out "depends=\"~{~A~^ ~}\"~%" depends)
      (format out "makedepends=\"\"~%")
      (format out "install=\"\"~%")
      (format out "source=\"\"~%")
      (format out "builddir=\"$srcdir\"~%~%")
      
      ;; Package functions
      (format out "build() {~%")
      (format out "    # Build commands go here~%")
      (format out "    echo \"Building $pkgname-$pkgver\"~%")
      (format out "}~%~%")
      
      (format out "check() {~%")
      (format out "    # Check commands go here~%")
      (format out "    echo \"Checking $pkgname-$pkgver\"~%")
      (format out "}~%~%")
      
      (format out "package() {~%")
      (format out "    # Install commands go here~%")
      (format out "    mkdir -p \"$pkgdir\"~%")
      (format out "    cp -r \"$startdir\"/* \"$pkgdir\"/~%")
      (format out "    rm \"$pkgdir\"/APKBUILD~%")
      (format out "}~%"))
    
    apkbuild-path))

(defun copy-package-sources (builder package-name source-dir build-dir)
  "Copy package source files to the build directory."
  (log-message :debug "Copying package sources from ~A to ~A" source-dir build-dir)
  
  ;; Create a temporary directory for source files
  (let ((src-temp-dir (path-join build-dir "src")))
    (ensure-directories-exist src-temp-dir)
    
    ;; Copy all files from source-dir to src-temp-dir
    (dolist (file (directory-files source-dir))
      (let ((src-file (path-join source-dir file))
            (dst-file (path-join src-temp-dir file)))
        (cond
          ((file-directory-p src-file)
           (copy-directory-recursive src-file dst-file))
          (t
           (copy-file src-file dst-file))))))
  
  t)

(defun build-package (builder package-name build-dir)
  "Build an APK package from the prepared build directory."
  (log-message :debug "Building package: ~A in ~A" package-name build-dir)
  
  ;; Change to build directory
  (let ((original-dir (getcwd)))
    (unwind-protect
        (progn
          (chdir build-dir)
          
          ;; Run abuild to create the package
          (let ((result (run-command-status 
                         "abuild" 
                         `("-P" ,(builder-output-dir builder) 
                           "-k" ,(path-join (builder-keys-dir builder) "private.rsa")
                           "-K" "-f"))))
            (if (= result 0)
                (log-message :info "Successfully built package: ~A" package-name)
                (log-message :error "Failed to build package: ~A (exit code: ~D)" 
                            package-name result))))
      
      ;; Restore original directory
      (chdir original-dir))))

(defun create-repository-index (builder repo-dir packages)
  "Create an APK repository index for the given packages."
  (log-message :info "Creating APK repository index in: ~A" repo-dir)
  
  ;; Ensure repository directory exists
  (ensure-directories-exist repo-dir)
  
  ;; Copy packages to repository
  (dolist (package packages)
    (let ((pkg-name (file-namestring package))
          (repo-package (path-join repo-dir (file-namestring package))))
      (copy-file package repo-package)))
  
  ;; Generate repository index
  (let ((original-dir (getcwd)))
    (unwind-protect
        (progn
          (chdir repo-dir)
          
          ;; Run apk index to create the repository
          (run-command "apk" 
                      `("index" 
                        "-o" ,(path-join repo-dir "APKINDEX.tar.gz")
                        "--allow-untrusted"
                        ,@(mapcar #'file-namestring packages))))
      
      ;; Restore original directory
      (chdir original-dir)))
  
  repo-dir)

;;; ----------------------------------------------------
;;; Utilities and Macros
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
;;; Default Variables
;;; ----------------------------------------------------

(defvar *default-build-root* "/tmp/onefilelinux-apk-build"
  "Default root directory for APK building.")

(defvar *default-config* nil
  "Default configuration context, should be set by the build system.")