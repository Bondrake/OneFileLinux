;;;; OneFileLinux Build Step: Configuration
;;;; Configures system services and settings
(defpackage :onefilelinux.build.conf
  (:use :cl :onefilelinux.core :onefilelinux.config :onefilelinux.build)
  (:export #:make-conf-step
           #:register-conf-step))

(in-package :onefilelinux.build.conf)

;;; ----------------------------------------------------
;;; Build Step Implementation - Configuration
;;; ----------------------------------------------------

(defclass conf-step (onefilelinux.build:build-step)
  ((name :initform :conf
         :allocation :class
         :reader build-step-name)
   (description :initform "Configures system services and settings"
                :allocation :class
                :reader build-step-description))
  (:documentation "Step to configure system services and settings."))

(defmethod prepare ((step conf-step) (context build-context))
  "Prepare for system configuration."
  (with-slots (working-dir) context
    (let ((rootfs-dir (path-join working-dir "rootfs")))
      ;; Validate required directories
      (unless (directory-exists-p rootfs-dir)
        (error 'onefilelinux-error 
               :message "Rootfs directory not found. Did previous steps complete successfully?"))
      
      ;; Ensure configuration directories exist
      (ensure-directories-exist (path-join rootfs-dir "etc"))
      (ensure-directories-exist (path-join rootfs-dir "etc/network"))
      (ensure-directories-exist (path-join rootfs-dir "root"))
      
      ;; Log preparation status
      (log-message :info "Prepared environment for system configuration")))
  t)

(defmethod execute ((step conf-step) (context build-context))
  "Execute system configuration."
  (with-slots (working-dir config) context
    (let ((rootfs-dir (path-join working-dir "rootfs"))
          (zfiles-dir (path-join working-dir "zfiles"))
          (profile (config-value config :build :profile "minimal")))
      
      ;; 1. Configure system files
      (log-message :info "Configuring system files...")
      (configure-system-files rootfs-dir zfiles-dir context)
      
      ;; 2. Configure network interfaces
      (log-message :info "Configuring network interfaces...")
      (configure-network-interfaces rootfs-dir config)
      
      ;; 3. Configure user environment
      (log-message :info "Configuring user environment...")
      (configure-user-environment rootfs-dir config)
      
      ;; 4. Configure root password
      (log-message :info "Configuring root password...")
      (configure-root-password rootfs-dir context)
      
      ;; 5. Configure kernel features
      (log-message :info "Configuring kernel features...")
      (configure-kernel-features working-dir config profile)
      
      ;; 6. Configure system services
      (log-message :info "Configuring system services...")
      (configure-system-services rootfs-dir config)
      
      (log-message :info "Successfully completed system configuration")))
  t)

(defmethod cleanup ((step conf-step) (context build-context) status)
  "Clean up after system configuration."
  (with-slots (working-dir config) context
    (let ((rootfs-dir (path-join working-dir "rootfs"))
          (preserve-work (config-value config :build :preserve-work nil)))
      
      ;; Remove temporary configuration files if not preserving work
      (unless preserve-work
        (let ((temp-configs (path-join rootfs-dir "tmp/configs")))
          (when (directory-exists-p temp-configs)
            (delete-directory-recursively temp-configs))))))
  t)

;;; ----------------------------------------------------
;;; Helper Functions - System Configuration
;;; ----------------------------------------------------

(defun configure-system-files (rootfs-dir zfiles-dir context)
  "Configure essential system files."
  ;; Copy essential system files from zfiles to rootfs
  (dolist (file '("init" "profile" "resolv.conf" "shadow"))
    (let ((src-file (path-join zfiles-dir file))
          (dst-file (path-join rootfs-dir "etc" file)))
      (when (file-exists-p src-file)
        (copy-file src-file dst-file)
        (log-message :debug "Copied ~A to ~A" src-file dst-file))))
  
  ;; Configure hostname
  (with-slots (config) context
    (let ((hostname (config-value config :system :hostname "onefilelinux"))
          (hostname-file (path-join rootfs-dir "etc/hostname")))
      (with-open-file (out hostname-file :direction :output 
                                        :if-exists :supersede 
                                        :if-does-not-exist :create)
        (format out "~A~%" hostname))
      (log-message :debug "Created hostname file with value: ~A" hostname))))

(defun configure-network-interfaces (rootfs-dir config)
  "Configure network interfaces."
  (let ((interfaces-file (path-join rootfs-dir "etc/network/interfaces"))
        (network-type (config-value config :system :network-type "dhcp")))
    
    ;; Create interfaces file based on configuration
    (with-open-file (out interfaces-file :direction :output 
                                        :if-exists :supersede 
                                        :if-does-not-exist :create)
      (format out "auto lo~%")
      (format out "iface lo inet loopback~%~%")
      (format out "auto eth0~%")
      
      ;; Configure based on network type
      (cond
        ((string= network-type "dhcp")
         (format out "iface eth0 inet dhcp~%"))
        ((string= network-type "static")
         (let ((ip (config-value config :system :ip "192.168.1.100"))
               (netmask (config-value config :system :netmask "255.255.255.0"))
               (gateway (config-value config :system :gateway "192.168.1.1")))
           (format out "iface eth0 inet static~%")
           (format out "    address ~A~%" ip)
           (format out "    netmask ~A~%" netmask)
           (format out "    gateway ~A~%" gateway)))
        (t
         (log-message :warning "Unknown network type '~A'. Defaulting to DHCP." network-type)
         (format out "iface eth0 inet dhcp~%"))))
    
    (log-message :debug "Created network interfaces file with ~A configuration" network-type)))

(defun configure-user-environment (rootfs-dir config)
  "Configure user environment settings."
  ;; Configure TUI if enabled
  (when (config-value config :features :tui t)
    (let ((src-file (path-join (context-working-dir *build-context*) "zfiles/onefilelinux-tui"))
          (dst-file (path-join rootfs-dir "usr/sbin/onefilelinux-tui")))
      (copy-file src-file dst-file)
      (chmod dst-file #o755)
      (log-message :debug "Installed OneFileLinux TUI")))
  
  ;; Configure profile.d scripts
  (let ((profile-d (path-join rootfs-dir "etc/profile.d")))
    (ensure-directories-exist profile-d)
    (with-open-file (out (path-join profile-d "onefilelinux.sh") 
                         :direction :output 
                         :if-exists :supersede 
                         :if-does-not-exist :create)
      (format out "#!/bin/sh~%")
      (format out "export PS1='OneFileLinux:\\w\\$ '~%")
      (format out "export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin~%")
      (when (config-value config :features :tui t)
        (format out "# Auto-start TUI on login if not in a subshell~%")
        (format out "if [ \"$SHLVL\" = 1 ]; then~%")
        (format out "    /usr/sbin/onefilelinux-tui~%")
        (format out "fi~%")))
    (chmod (path-join profile-d "onefilelinux.sh") #o755)))

(defun configure-root-password (rootfs-dir context)
  "Configure root password."
  (with-slots (config) context
    (let ((shadow-file (path-join rootfs-dir "etc/shadow"))
          (password (or (config-value config :system :root-password nil)
                       (generate-root-password context))))
      
      ;; Set root password in shadow file
      (when (file-exists-p shadow-file)
        (let ((shadow-contents (read-file-as-string shadow-file))
              (hashed-password (crypt-password password)))
          (let ((new-contents (regex-replace 
                               "^root::[^:]*:[^:]*:[^:]*:[^:]*:[^:]*:[^:]*:[^:]*$"
                               shadow-contents
                               (format nil "root:~A:0:0:99999:7:::" hashed-password))))
            (write-string-to-file new-contents shadow-file)))
        
        (log-message :info "Root password configured")))))

(defun generate-root-password (context)
  "Generate a random root password and save it for later retrieval."
  (let ((password (generate-random-string 12))
        (password-file (path-join (context-working-dir context) "onefilelinux-password.txt")))
    
    ;; Save password to file for later retrieval
    (with-open-file (out password-file :direction :output 
                                      :if-exists :supersede 
                                      :if-does-not-exist :create)
      (format out "OneFileLinux Root Password: ~A~%" password))
    (chmod password-file #o600)
    
    (log-message :info "Generated random root password and saved to ~A" password-file)
    password))

(defun crypt-password (password)
  "Create a hashed password using crypt algorithm."
  (let ((salt (generate-random-string 8)))
    ;; In a real implementation, this would use a proper crypt function
    ;; For now, we use a stub that would be replaced with actual implementation
    (format nil "$6$~A$~A" salt (generate-md5-hash (concatenate 'string salt password)))))

(defun generate-random-string (length)
  "Generate a random alphanumeric string of specified length."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat length
                  collect (aref chars (random (length chars))))
            'string)))

(defun generate-md5-hash (string)
  "Generate an MD5 hash of the input string."
  ;; This is a stub that would be replaced with actual MD5 implementation
  ;; In a real implementation, this would use a proper MD5 function
  (subseq (format nil "~36R" (reduce #'+ (map 'list #'char-code string))) 0 32))

;;; ----------------------------------------------------
;;; Helper Functions - Kernel Configuration
;;; ----------------------------------------------------

(defun configure-kernel-features (working-dir config profile)
  "Configure kernel with feature overlays."
  (let* ((kernel-config-base (path-join working-dir "kernel/config"))
         (kernel-config-overlays (path-join working-dir "kernel-configs/features"))
         (enabled-features (config-value config :features :enabled nil)))
    
    ;; Copy base kernel config
    (ensure-directories-exist (path-join working-dir "kernel"))
    (let ((profile-config (path-join working-dir 
                                    (format nil "kernel-configs/~A.config" profile))))
      (if (file-exists-p profile-config)
          (copy-file profile-config kernel-config-base)
          (error 'onefilelinux-error 
                 :message (format nil "Kernel config for profile '~A' not found" profile))))
    
    ;; Apply feature overlays
    (dolist (feature enabled-features)
      (let ((feature-overlay (path-join kernel-config-overlays 
                                       (format nil "~A.conf" feature))))
        (when (file-exists-p feature-overlay)
          (log-message :debug "Applying kernel feature overlay: ~A" feature)
          (apply-kernel-config-overlay kernel-config-base feature-overlay))))
    
    (log-message :info "Kernel configuration complete with ~D features applied" 
                (length enabled-features))))

(defun apply-kernel-config-overlay (base-config overlay-config)
  "Apply a kernel configuration overlay to a base config."
  ;; This function would use the kernel-config-utils to apply overlays
  ;; For now, we provide a simple implementation that would be replaced
  (let ((base-content (read-file-as-string base-config))
        (overlay-content (read-file-as-string overlay-config)))
    
    ;; Process each line in the overlay
    (with-input-from-string (stream overlay-content)
      (let ((new-content base-content))
        (loop for line = (read-line stream nil nil)
              while line
              do (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                   (unless (or (string= trimmed "") 
                               (char= (char trimmed 0) #\#))
                     (let* ((parts (split-string trimmed "="))
                            (key (first parts)))
                       ;; Replace or add config option
                       (if (search key new-content)
                           (setf new-content 
                                 (regex-replace 
                                  (format nil "~A=.*" key)
                                  new-content
                                  trimmed))
                           (setf new-content 
                                 (concatenate 'string new-content 
                                             (format nil "~%~A" trimmed))))))))
        (write-string-to-file new-content base-config)))))

;;; ----------------------------------------------------
;;; Helper Functions - System Services
;;; ----------------------------------------------------

(defun configure-system-services (rootfs-dir config)
  "Configure system services based on configuration."
  ;; Configure runlevels
  (ensure-directories-exist (path-join rootfs-dir "etc/runlevels/default"))
  
  ;; Add services to runlevels based on configuration
  (with-chroot rootfs-dir
    ;; Add default services
    (dolist (service '("networking" "crond" "syslog"))
      (run-command "rc-update" `("add" ,service "default") :capture-output nil))
    
    ;; Configure SSH if enabled
    (when (config-value config :features :ssh-server t)
      (run-command "rc-update" '("add" "sshd" "default") :capture-output nil)
      (log-message :debug "Added SSH server to default runlevel"))
    
    ;; Configure firewall if enabled
    (when (config-value config :features :firewall t)
      (run-command "rc-update" '("add" "iptables" "default") :capture-output nil)
      (log-message :debug "Added firewall to default runlevel"))))

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

(defun make-conf-step ()
  "Create a new instance of the conf-step."
  (make-instance 'conf-step))

(defun register-conf-step ()
  "Register the conf step with the build system."
  (onefilelinux.build:register-build-step (make-conf-step)))

;; Auto-register when the package is loaded
(eval-when (:load-toplevel :execute)
  (register-conf-step))