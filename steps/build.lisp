;;;; OneFileLinux Build Step: Build
;;;; Builds the kernel and creates the final EFI file
(defpackage :onefilelinux.build.build
  (:use :cl :onefilelinux.core :onefilelinux.config :onefilelinux.build
       :onefilelinux.kernel.config-utils :onefilelinux.package.apk-builder)
  (:export #:make-build-step
           #:register-build-step))

(in-package :onefilelinux.build.build)

;;; ----------------------------------------------------
;;; Build Step Implementation - Build
;;; ----------------------------------------------------

(defclass build-kernel-step (onefilelinux.build:build-step)
  ((name :initform :build
         :allocation :class
         :reader build-step-name)
   (description :initform "Builds the kernel and creates the final EFI file"
                :allocation :class
                :reader build-step-description))
  (:documentation "Step to build the kernel and create the final EFI file."))

(defmethod prepare ((step build-kernel-step) (context build-context))
  "Prepare for kernel building and EFI creation."
  (with-slots (working-dir) context
    (let ((rootfs-dir (path-join working-dir "rootfs"))
          (kernel-dir (path-join working-dir "kernel"))
          (output-dir (path-join working-dir "output")))
      
      ;; Validate required directories
      (unless (directory-exists-p rootfs-dir)
        (error 'onefilelinux-error 
               :message "Rootfs directory not found. Did previous steps complete successfully?"))
      
      (unless (directory-exists-p kernel-dir)
        (error 'onefilelinux-error 
               :message "Kernel directory not found. Did previous steps complete successfully?"))
      
      ;; Create output directory
      (ensure-directories-exist output-dir)
      
      ;; Log preparation status
      (log-message :info "Prepared environment for kernel building and EFI creation")))
  t)

(defmethod execute ((step build-kernel-step) (context build-context))
  "Execute kernel building and EFI creation."
  (with-slots (working-dir config) context
    (let ((rootfs-dir (path-join working-dir "rootfs"))
          (kernel-dir (path-join working-dir "kernel"))
          (output-dir (path-join working-dir "output"))
          (profile (config-value config :build :profile "minimal"))
          (custom-packages (config-value config :build :custom-packages nil)))
      
      ;; 1. Build custom packages if enabled
      (when custom-packages
        (log-message :info "Building custom packages...")
        (build-custom-packages context custom-packages))
      
      ;; 2. Build the kernel
      (log-message :info "Building the Linux kernel...")
      (build-kernel context kernel-dir rootfs-dir)
      
      ;; 3. Create initramfs
      (log-message :info "Creating initramfs...")
      (create-initramfs context rootfs-dir)
      
      ;; 4. Create EFI file
      (log-message :info "Creating EFI file...")
      (create-efi-file context kernel-dir rootfs-dir output-dir)
      
      ;; 5. Compress if enabled
      (when (config-value config :build :compress-output t)
        (log-message :info "Compressing output...")
        (compress-output context output-dir))
      
      (log-message :info "Successfully completed build process")))
  t)

(defmethod cleanup ((step build-kernel-step) (context build-context) status)
  "Clean up after kernel building and EFI creation."
  (with-slots (working-dir config) context
    (let ((preserve-work (config-value config :build :preserve-work nil)))
      
      ;; Clean up temporary files if not preserving work
      (unless preserve-work
        (let ((kernel-build-dir (path-join working-dir "kernel/build"))
              (package-build-dir (path-join working-dir "package/build")))
          
          ;; Remove kernel build directory
          (when (directory-exists-p kernel-build-dir)
            (log-message :debug "Cleaning kernel build directory...")
            (delete-directory-recursively kernel-build-dir))
          
          ;; Remove package build directory
          (when (directory-exists-p package-build-dir)
            (log-message :debug "Cleaning package build directory...")
            (delete-directory-recursively package-build-dir))))))
  t)

;;; ----------------------------------------------------
;;; Helper Functions - Package Building
;;; ----------------------------------------------------

(defun build-custom-packages (context packages)
  "Build custom packages for the OneFileLinux system."
  (with-slots (working-dir config) context
    (let ((package-dir (path-join working-dir "package"))
          (rootfs-dir (path-join working-dir "rootfs")))
      
      ;; Ensure package directories exist
      (ensure-directories-exist (path-join package-dir "sources"))
      (ensure-directories-exist (path-join package-dir "build"))
      (ensure-directories-exist (path-join package-dir "repo"))
      
      ;; Build package repository
      (let ((repo-dir (build-package-repository packages
                                              :build-root package-dir
                                              :config config)))
        (when repo-dir
          ;; Install custom packages into rootfs
          (install-custom-packages rootfs-dir repo-dir
                                  :packages (mapcar (lambda (pkg) 
                                                     (if (listp pkg) (first pkg) pkg))
                                                   packages)
                                  :config config)
          
          (log-message :info "Successfully built and installed ~D custom packages" 
                      (length packages)))))))

;;; ----------------------------------------------------
;;; Helper Functions - Kernel Building
;;; ----------------------------------------------------

(defun build-kernel (context kernel-dir rootfs-dir)
  "Build the Linux kernel from source."
  (with-slots (config) context
    (let* ((kernel-source (path-join kernel-dir "source"))
           (kernel-config (path-join kernel-dir "config"))
           (jobs (config-value config :build :make-jobs
                               (determine-optimal-jobs))))
      
      ;; Check that kernel source exists
      (unless (directory-exists-p kernel-source)
        (error 'onefilelinux-error 
               :message "Kernel source directory not found. Did get step complete successfully?"))
      
      ;; Check that kernel config exists
      (unless (file-exists-p kernel-config)
        (error 'onefilelinux-error 
               :message "Kernel config not found. Did configuration step complete successfully?"))
      
      ;; Copy kernel config to source directory
      (copy-file kernel-config (path-join kernel-source ".config"))
      
      ;; Build kernel
      (let ((original-dir (getcwd)))
        (unwind-protect
            (progn
              (chdir kernel-source)
              
              ;; Run make olddefconfig to ensure config is valid
              (log-message :debug "Validating kernel config...")
              (run-command "make" '("olddefconfig"))
              
              ;; Build kernel
              (log-message :debug "Building kernel with ~D jobs..." jobs)
              (run-command "make" `("-j" ,(format nil "~D" jobs)))
              
              ;; Install kernel modules to rootfs
              (log-message :debug "Installing kernel modules...")
              (run-command "make" `("INSTALL_MOD_PATH=~A" ,rootfs-dir "modules_install"))
              
              ;; Install kernel firmware to rootfs
              (log-message :debug "Installing kernel firmware...")
              (run-command "make" `("INSTALL_FW_PATH=~A/lib/firmware" ,rootfs-dir "firmware_install")))
          
          ;; Restore original directory
          (chdir original-dir))))))

(defun determine-optimal-jobs ()
  "Determine the optimal number of jobs for parallel building."
  (let ((cpu-count (get-cpu-count)))
    (max 1 (floor (* cpu-count 0.75)))))

(defun get-cpu-count ()
  "Get the number of CPU cores available."
  (let ((result (run-command-output "nproc" '())))
    (parse-integer (string-trim '(#\Space #\Tab #\Newline) result))))

;;; ----------------------------------------------------
;;; Helper Functions - Initramfs Creation
;;; ----------------------------------------------------

(defun create-initramfs (context rootfs-dir)
  "Create an initramfs for booting."
  (with-slots (config) context
    (log-message :debug "Creating initramfs using mkinitfs...")
    
    ;; Create mkinitfs configuration
    (let ((mkinitfs-config (path-join rootfs-dir "etc/mkinitfs/mkinitfs.conf")))
      (ensure-directories-exist (path-join rootfs-dir "etc/mkinitfs"))
      
      ;; Write mkinitfs configuration
      (with-open-file (out mkinitfs-config :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
        (format out "features=\"base\"~%")
        
        ;; Add features based on configuration
        (let ((enabled-features (config-value config :features :enabled nil)))
          (when enabled-features
            (let ((initfs-features '()))
              ;; Map OneFileLinux features to mkinitfs features
              (when (member "crypto-support" enabled-features :test #'string=)
                (push "cryptsetup" initfs-features))
              (when (member "network-boot" enabled-features :test #'string=)
                (push "network" initfs-features))
              (when (member "lvm-support" enabled-features :test #'string=)
                (push "lvm" initfs-features))
              
              ;; Add to configuration if any features were mapped
              (when initfs-features
                (format out "features=\"$features ~{~A~^ ~}\"~%" initfs-features))))))
      
      ;; Run mkinitfs in chroot
      (with-chroot rootfs-dir
        (run-command "mkinitfs" '("-o" "/boot/initramfs-lts" "$(ls /lib/modules)"))))))

;;; ----------------------------------------------------
;;; Helper Functions - EFI Creation
;;; ----------------------------------------------------

(defun create-efi-file (context kernel-dir rootfs-dir output-dir)
  "Create the final EFI bootable file."
  (with-slots (config) context
    (let* ((kernel-source (path-join kernel-dir "source"))
           (kernel-binary (path-join kernel-source "arch/x86/boot/bzImage"))
           (initramfs (path-join rootfs-dir "boot/initramfs-lts"))
           (profile (config-value config :build :profile "minimal"))
           (efi-output (path-join output-dir 
                               (format nil "onefilelinux-~A.efi" profile))))
      
      ;; Check that kernel binary exists
      (unless (file-exists-p kernel-binary)
        (error 'onefilelinux-error 
               :message "Kernel binary not found. Did kernel build complete successfully?"))
      
      ;; Check that initramfs exists
      (unless (file-exists-p initramfs)
        (error 'onefilelinux-error 
               :message "Initramfs not found. Did initramfs creation complete successfully?"))
      
      ;; Create EFI file using objcopy
      (log-message :debug "Creating EFI file: ~A" efi-output)
      (run-command "objcopy" 
                  `("--add-section" ,(format nil ".osrel=~A" (create-os-release context))
                    "--change-section-vma" ".osrel=0x20000"
                    "--add-section" ,(format nil ".cmdline=~A" (create-cmdline context))
                    "--change-section-vma" ".cmdline=0x30000"
                    "--add-section" ,(format nil ".linux=~A" kernel-binary)
                    "--change-section-vma" ".linux=0x40000"
                    "--add-section" ,(format nil ".initrd=~A" initramfs)
                    "--change-section-vma" ".initrd=0x3000000"
                    "/usr/lib/systemd/boot/efi/linuxx64.efi.stub"
                    ,efi-output))
      
      ;; Set permissions on EFI file
      (chmod efi-output #o755)
      
      (log-message :info "Created EFI file: ~A" efi-output)
      efi-output)))

(defun create-os-release (context)
  "Create OS release information file."
  (with-slots (config working-dir) context
    (let ((os-release-file (path-join working-dir "tmp/os-release")))
      (ensure-directories-exist (path-join working-dir "tmp"))
      
      ;; Write OS release information
      (with-open-file (out os-release-file :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
        (format out "NAME=\"OneFileLinux\"~%")
        (format out "ID=onefilelinux~%")
        (format out "VERSION_ID=~A~%" (config-value config :system :version "1.0.0"))
        (format out "PRETTY_NAME=\"OneFileLinux ~A (~A)\"~%" 
               (config-value config :system :version "1.0.0")
               (config-value config :build :profile "minimal")))
      
      os-release-file)))

(defun create-cmdline (context)
  "Create kernel command line arguments file."
  (with-slots (config working-dir) context
    (let ((cmdline-file (path-join working-dir "tmp/cmdline")))
      (ensure-directories-exist (path-join working-dir "tmp"))
      
      ;; Write kernel command line
      (with-open-file (out cmdline-file :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
        (let ((cmdline (or (config-value config :system :cmdline nil)
                          "console=tty0 console=ttyS0,115200 quiet")))
          (format out "~A" cmdline)))
      
      cmdline-file)))

;;; ----------------------------------------------------
;;; Helper Functions - Output Compression
;;; ----------------------------------------------------

(defun compress-output (context output-dir)
  "Compress the output EFI file for distribution."
  (with-slots (config) context
    (let* ((profile (config-value config :build :profile "minimal"))
           (efi-file (path-join output-dir (format nil "onefilelinux-~A.efi" profile)))
           (compressed-file (path-join output-dir 
                                     (format nil "onefilelinux-~A.efi.xz" profile))))
      
      ;; Check that EFI file exists
      (unless (file-exists-p efi-file)
        (error 'onefilelinux-error 
               :message "EFI file not found. Cannot compress output."))
      
      ;; Compress using xz
      (log-message :debug "Compressing EFI file...")
      (run-command "xz" `("-f" "-9" "-k" ,efi-file))
      
      (log-message :info "Created compressed output: ~A" compressed-file)
      compressed-file)))

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
;;; Build Step Registration
;;; ----------------------------------------------------

(defun make-build-step ()
  "Create a new instance of the build-kernel-step."
  (make-instance 'build-kernel-step))

(defun register-build-kernel-step ()
  "Register the build step with the build system."
  (onefilelinux.build:register-build-step (make-build-step)))

;; Auto-register when the package is loaded
(eval-when (:load-toplevel :execute)
  (register-build-kernel-step))