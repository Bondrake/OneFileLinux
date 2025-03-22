;;;; OneFileLinux Configuration System
;;;; Modular configuration management for the build system

(defpackage :onefilelinux.config
  (:use :cl :onefilelinux.core)
  (:export
   ;; Configuration management
   #:load-config
   #:save-config
   #:config-value
   #:set-config-value
   #:with-config-overrides
   #:register-config-schema
   #:validate-config
   
   ;; Configuration accessors
   #:system-config
   #:build-config
   #:feature-config
   #:package-config
   
   ;; Build profiles
   #:list-profiles
   #:profile-exists-p
   #:get-profile
   #:apply-profile
   
   ;; Package groups
   #:list-package-groups
   #:resolve-package-groups
   #:package-group-exists-p
   #:register-package-group
   
   ;; Constants
   #:*alpine-version*
   #:*kernel-version*
   #:*default-profile*
   
   ;; Configuration schema types for extension
   #:define-config-schema
   #:define-config-section))

(in-package :onefilelinux.config)

;;; ===============================
;;; Core Version Information
;;; ===============================

(defparameter *alpine-version* "3.21"
  "Alpine Linux version used in the build")

(defparameter *kernel-version* "6.12.19"
  "Linux kernel version used in the build")

(defparameter *default-profile* "standard"
  "Default build profile")

;;; ===============================
;;; Configuration Schema System
;;; ===============================

(defstruct config-schema
  "Schema for configuration validation"
  (name nil :type symbol)
  (fields nil :type list)
  (validators nil :type list))

(defparameter *config-schemas* (make-hash-table :test 'eq)
  "Registry of configuration schemas")

(defmacro define-config-schema (name &body fields)
  "Define a configuration schema"
  ;; Handle the case where first element is a documentation string
  (let ((doc-string nil)
        (actual-fields fields))
    (when (and (stringp (first fields)) (rest fields))
      (setf doc-string (first fields)
            actual-fields (rest fields)))
    
    `(progn
       (defstruct ,name
         ,@(when doc-string (list doc-string))
         ,@(mapcar (lambda (field-def)
                    (destructuring-bind (field-name &key type default documentation)
                        field-def
                      (declare (ignore type))
                      `(,field-name ,default :read-only nil
                                  :documentation ,documentation)))
                  actual-fields))
       
       (setf (gethash ',name *config-schemas*)
             (make-config-schema 
              :name ',name
              :fields ',(mapcar (lambda (field-def)
                                (destructuring-bind (field-name &key type default documentation)
                                    field-def
                                  (declare (ignore default))
                                  (list field-name type documentation)))
                              actual-fields)
              :validators nil))
       ',name)))

(defmacro define-config-section (name parent &body fields)
  "Define a configuration section that inherits from another"
  `(progn
     (defstruct (,name (:include ,parent))
       ,@(mapcar (lambda (field-def)
                  (destructuring-bind (field-name &key type default documentation)
                      field-def
                    (declare (ignore type))
                    `(,field-name ,default :read-only nil
                                :documentation ,documentation)))
                fields))
     
     (setf (gethash ',name *config-schemas*)
           (make-config-schema 
            :name ',name
            :fields (append 
                     (config-schema-fields 
                      (gethash ',parent *config-schemas*))
                     ',(mapcar (lambda (field-def)
                               (destructuring-bind (field-name &key type default documentation)
                                   field-def
                                 (declare (ignore default))
                                 (list field-name type documentation)))
                             fields))
            :validators nil))
     ',name))

(defun register-config-schema (schema-name)
  "Register a configuration schema"
  (unless (gethash schema-name *config-schemas*)
    (error 'configuration-error
          :message (format nil "Unknown schema: ~A" schema-name)))
  schema-name)

;;; ===============================
;;; Configuration Base Schema
;;; ===============================

(define-config-schema config-base
  (version :type string 
          :default "1.0.0"
          :documentation "Configuration version"))

;;; ===============================
;;; System Configuration
;;; ===============================

(define-config-section system-config config-base
  (alpine-version :type string 
                 :default *alpine-version*
                 :documentation "Alpine Linux version")
  
  (kernel-version :type string 
                 :default *kernel-version*
                 :documentation "Linux kernel version")
  
  (alpine-minirootfs-file :type string 
                         :default (format nil "alpine-minirootfs-~A.3-x86_64.tar.gz" *alpine-version*)
                         :documentation "Alpine minirootfs filename")
  
  (linux-version-string :type string 
                       :default (format nil "linux-~A" *kernel-version*)
                       :documentation "Linux kernel version string")
  
  (download-urls :type list 
                :default '(:alpine "http://dl-cdn.alpinelinux.org/alpine/v~a/releases/x86_64/~a"
                          :kernel "http://cdn.kernel.org/pub/linux/kernel/v6.x/~a.tar.xz"
                          :github "https://github.com/~a/~a/archive/refs/tags/~a.tar.gz"
                          :gitlab "https://gitlab.com/~a/~a/-/archive/~a/~a-~a.tar.gz")
                :documentation "URLs for downloading components")
  
  (artifact-paths :type list
                 :default '(:downloads     "build/downloads"
                           :extracted     "build/sources"
                           :work          "build/work"
                           :ccache        "build/ccache"
                           :packages      "build/packages"
                           :custom-apks   "build/custom-apks"
                           :logs          "build/logs"
                           :output        "output"
                           :temp          "build/temp")
                 :documentation "Paths for build artifacts"))

;;; ===============================
;;; Build Configuration
;;; ===============================

(define-config-section build-config config-base
  (profile :type string 
          :default "standard"
          :documentation "Build profile")
  
  (clean-start :type boolean 
              :default nil
              :documentation "Clean before building")
  
  (clean-end :type boolean 
            :default nil
            :documentation "Clean after building")
  
  (verbose :type boolean 
          :default nil
          :documentation "Enable verbose output")
  
  (build-jobs :type integer 
             :default 4
             :documentation "Number of parallel build jobs")
  
  (use-cache :type boolean 
            :default t
            :documentation "Use build cache")
  
  (cache-dir :type string 
            :default "~/.onefilelinux/cache"
            :documentation "Cache directory")
  
  (use-swap :type boolean 
           :default nil
           :documentation "Use swap space during build")
  
  (interactive-config :type boolean 
                     :default nil
                     :documentation "Use interactive configuration")
  
  (root-password :type (or string null) 
                :default nil
                :documentation "Root password for the built system")
  
  (generate-random-password :type boolean 
                           :default t
                           :documentation "Generate random root password"))

;;; ===============================
;;; Build Requirements Configuration
;;; ===============================

(define-config-schema build-requirements-config
  "Configuration for build requirements and dependencies"
  
  (required-tools :type list
                 :default ("make" "gcc" "g++" "ld")
                 :documentation "List of required build tools")
  
  (chroot-tools :type list
               :default ("chroot" "mount" "umount")
               :documentation "List of tools required for chroot operations")
  
  (download-tools :type list
                 :default ("wget" "curl" "tar" "xz")
                 :documentation "List of tools required for downloading and extracting")
  
  (strict-tool-check :type boolean
                    :default nil
                    :documentation "Enforce strict tool requirements check")
  
  (require-root :type boolean
               :default t
               :documentation "Whether root privileges are required")
  
  (strict-root-check :type boolean
                    :default nil
                    :documentation "Enforce strict root requirement check"))

;;; ===============================
;;; Docker Configuration
;;; ===============================

(define-config-schema docker-config
  "Configuration for Docker build environment"
  
  (container-root :type string
                 :default "/build"
                 :documentation "Root directory inside Docker container")
  
  (output-dir :type string
             :default "/output"
             :documentation "Output directory inside Docker container")
  
  (log-file :type string
           :default "/var/log/onefilelinux-build.log"
           :documentation "Log file location inside Docker container")
  
  (sbcl-path :type string
            :default "/usr/bin/sbcl"
            :documentation "Path to SBCL inside Docker container")
  
  (entrypoint-options :type list
                     :default ("--no-sysinit" "--no-userinit")
                     :documentation "Options for SBCL in Docker entrypoint")
  
  (auto-detect-resources :type boolean
                        :default t
                        :documentation "Automatically detect and limit resource usage")
  
  (default-cpu-limit :type integer
                    :default 2
                    :documentation "Default CPU limit if not specified")
  
  (default-memory-limit :type integer
                       :default 4096
                       :documentation "Default memory limit in MB if not specified"))

;;; ===============================
;;; Feature Configuration
;;; ===============================

(define-config-section feature-config config-base
  (include-zfs :type boolean 
              :default t
              :documentation "Include ZFS support")
  
  (include-btrfs :type boolean 
                :default nil
                :documentation "Include BTRFS support")
  
  (include-recovery-tools :type boolean 
                         :default t
                         :documentation "Include recovery tools")
  
  (include-network-tools :type boolean 
                        :default t
                        :documentation "Include network tools")
  
  (include-crypto :type boolean 
                 :default t
                 :documentation "Include crypto support")
  
  (include-tui :type boolean 
              :default t
              :documentation "Include TUI interface")
  
  (include-minimal-kernel :type boolean 
                         :default nil
                         :documentation "Use minimal kernel config")
  
  (include-compression :type boolean 
                      :default t
                      :documentation "Enable EFI compression")
  
  (compression-tool :type string 
                   :default "upx"
                   :documentation "Compression tool (upx, xz, zstd)")
  
  (enable-custom-apks :type boolean 
                     :default t
                     :documentation "Enable custom APK building"))

;;; ===============================
;;; Package Configuration
;;; ===============================

(define-config-section package-config config-base
  (package-groups :type list 
                 :default nil
                 :documentation "Package groups to include")
  
  (extra-packages :type list 
                 :default nil
                 :documentation "Extra packages to include")
  
  (custom-apk-sources :type string 
                     :default nil
                     :documentation "Custom APK source directory")
  
  (custom-apk-list :type list 
                  :default nil
                  :documentation "Custom APKs to build")
  
  (skip-apk-build :type boolean 
                 :default nil
                 :documentation "Skip APK building")
  
  (force-rebuild-apks :type boolean 
                     :default nil
                     :documentation "Force APK rebuilding"))

;;; ===============================
;;; Configuration State
;;; ===============================

(defparameter *config* nil
  "Global configuration state")

(defparameter *config-overrides* nil
  "Dynamic configuration overrides")

;;; ===============================
;;; Configuration Operations
;;; ===============================

(defun initialize-config ()
  "Initialize the configuration with defaults"
  (unless *config*
    (setf *config* (make-hash-table :test 'eq))
    (setf (gethash 'system-config *config*) (make-system-config))
    (setf (gethash 'build-config *config*) (make-build-config))
    (setf (gethash 'feature-config *config*) (make-feature-config))
    (setf (gethash 'package-config *config*) (make-package-config))))

(defun config-value (section slot &optional default)
  "Get a configuration value"
  (initialize-config)
  (let ((section-override (and *config-overrides* (getf *config-overrides* section)))
        (section-config (gethash section *config*)))
    
    (cond
      ;; Check overrides first
      ((and section-override (slot-exists-p section-override slot))
       (slot-value section-override slot))
      
      ;; Then check main config
      ((and section-config (slot-exists-p section-config slot))
       (slot-value section-config slot))
      
      ;; Fall back to default
      (t default))))

(defun set-config-value (section slot value)
  "Set a configuration value"
  (initialize-config)
  (let ((section-config (gethash section *config*)))
    (unless section-config
      (error 'configuration-error
            :message (format nil "Unknown configuration section: ~A" section)))
    
    (unless (slot-exists-p section-config slot)
      (error 'configuration-error
            :message (format nil "Unknown configuration slot ~A in section ~A" 
                           slot section)))
    
    (setf (slot-value section-config slot) value)))

(defmacro with-config-overrides ((&rest overrides) &body body)
  "Execute body with temporary configuration overrides"
  (let ((old-overrides (gensym "old-overrides-")))
    `(let ((,old-overrides *config-overrides*))
       (setf *config-overrides* (list ,@overrides))
       (unwind-protect
            (progn ,@body)
         (setf *config-overrides* ,old-overrides)))))

(defun validate-config-section (section)
  "Validate a configuration section against its schema"
  (initialize-config)
  (let ((section-config (gethash section *config*))
        (schema (gethash section *config-schemas*)))
    
    (unless section-config
      (error 'configuration-error
            :message (format nil "Missing configuration section: ~A" section)))
    
    (unless schema
      (error 'configuration-error
            :message (format nil "No schema defined for section: ~A" section)))
    
    ;; Check each field against its type constraint
    (loop for (field-name field-type _) in (config-schema-fields schema)
          for slot-value = (slot-value section-config field-name)
          unless (or (null field-type) 
                    (eql field-type t)
                    (and (listp field-type)
                         (eql (first field-type) 'or)
                         (some (lambda (type) 
                                (typep slot-value type))
                              (rest field-type)))
                    (typep slot-value field-type))
          collect (format nil "Field ~A in ~A has invalid type: expected ~A, got ~A"
                        field-name section field-type (type-of slot-value)))))

(defun validate-config ()
  "Validate the entire configuration"
  (initialize-config)
  (let ((errors '()))
    (dolist (section '(system-config build-config feature-config package-config))
      (let ((section-errors (validate-config-section section)))
        (when section-errors
          (setf errors (append errors section-errors)))))
    
    (if errors
        (values nil errors)
        (values t nil))))

(defun load-config (file)
  "Load configuration from a file"
  (when (file-exists-p file)
    (with-open-file (in file :direction :input)
      (let ((config (read in)))
        (initialize-config)
        
        ;; Load each section
        (dolist (section '(system-config build-config feature-config package-config))
          (when (getf config section)
            (let ((section-data (getf config section))
                  (section-obj (gethash section *config*)))
              
              ;; Load each key in the section
              (loop for (key value) on section-data by #'cddr
                    when (slot-exists-p section-obj key)
                    do (setf (slot-value section-obj key) value)))))
        
        ;; Validate the loaded configuration
        (multiple-value-bind (valid errors) (validate-config)
          (unless valid
            (error 'configuration-error
                  :message (format nil "Invalid configuration: ~{~A~^, ~}" errors))))
        
        t))))

(defun save-config (file)
  "Save configuration to a file"
  (initialize-config)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (let ((output '()))
      ;; Build configuration plist
      (dolist (section '(system-config build-config feature-config package-config))
        (let ((section-obj (gethash section *config*))
              (section-data '()))
          
          ;; Get all slots and values
          (do-slots (slot section-obj)
            (push (slot-value section-obj slot) section-data)
            (push slot section-data))
          
          ;; Add section to output
          (push (nreverse section-data) output)
          (push section output)))
      
      ;; Write to file
      (pprint (nreverse output) out)))
  t)

;;; ===============================
;;; Package Group System
;;; ===============================

(defstruct package-group
  "Definition of a package group"
  (name nil :type symbol)
  (description "" :type string)
  (packages nil :type list)
  (always-include nil :type boolean)
  (conflicts nil :type list)
  (dependencies nil :type list))

(defparameter *package-groups* (make-hash-table :test 'eq)
  "Registry of package groups")

(defun register-package-group (name &key description packages always-include conflicts dependencies)
  "Register a package group definition"
  (setf (gethash name *package-groups*)
        (make-package-group
         :name name
         :description description
         :packages packages
         :always-include always-include
         :conflicts conflicts
         :dependencies dependencies))
  name)

(defun package-group-exists-p (name)
  "Check if a package group exists"
  (not (null (gethash name *package-groups*))))

(defun get-package-group (name)
  "Get a package group by name"
  (or (gethash name *package-groups*)
      (error 'configuration-error
            :message (format nil "Unknown package group: ~A" name)
            :key name)))

(defun list-package-groups ()
  "Get a list of all package groups"
  (let ((groups '()))
    (maphash (lambda (name group)
               (declare (ignore name))
               (push group groups))
             *package-groups*)
    (sort groups #'string< :key (lambda (group) (symbol-name (package-group-name group))))))

(defun resolve-package-groups (group-names &optional include-dependencies)
  "Convert group names to a list of packages, resolving dependencies and conflicts"
  (let ((packages '())
        (processed-groups '())
        (groups-to-process group-names))
    
    ;; Include base packages if not explicitly excluded
    (unless (member :no-base group-names)
      (when (package-group-exists-p :base)
        (pushnew :base groups-to-process)))
    
    ;; Process meta-groups first (minimal, standard, full)
    (dolist (meta-group '(:minimal :standard :full))
      (when (and (member meta-group group-names)
                (package-group-exists-p meta-group))
        (let* ((meta-group-obj (get-package-group meta-group))
               (meta-deps (package-group-dependencies meta-group-obj)))
          (when meta-deps
            (setf groups-to-process 
                  (append (remove meta-group groups-to-process) meta-deps))))))
    
    ;; Process all groups
    (loop
      (let ((current-group (pop groups-to-process)))
        ;; Exit when all groups are processed
        (unless current-group (return))
        
        ;; Skip if already processed
        (when (member current-group processed-groups)
          (return))
        
        ;; Find group and get its data
        (when (package-group-exists-p current-group)
          (let ((group-obj (get-package-group current-group)))
            ;; Add to processed list
            (push current-group processed-groups)
            
            ;; Add direct packages from this group
            (let ((group-packages (package-group-packages group-obj)))
              (when group-packages
                (dolist (pkg group-packages)
                  (pushnew pkg packages :test #'string=))))
            
            ;; Add dependencies if requested
            (when include-dependencies
              (let ((dependencies (package-group-dependencies group-obj)))
                (when dependencies
                  (dolist (dep dependencies)
                    (unless (member dep processed-groups)
                      (push dep groups-to-process))))))))))
    
    ;; Return the sorted list of packages
    (sort packages #'string<)))

;;; ===============================
;;; Build Profiles
;;; ===============================

(defstruct build-profile
  "Definition of a build profile"
  (name nil :type string)
  (description "" :type string)
  (features nil :type list)
  (package-groups nil :type list)
  (output-name "" :type string))

(defparameter *build-profiles* (make-hash-table :test 'equal)
  "Registry of build profiles")

(defun register-build-profile (name &key description features package-groups output-name)
  "Register a build profile"
  (setf (gethash name *build-profiles*)
        (make-build-profile
         :name name
         :description description
         :features features
         :package-groups package-groups
         :output-name output-name))
  name)

(defun profile-exists-p (name)
  "Check if a build profile exists"
  (not (null (gethash name *build-profiles*))))

(defun get-profile (name)
  "Get a build profile by name"
  (or (gethash name *build-profiles*)
      (error 'configuration-error
            :message (format nil "Unknown build profile: ~A" name)
            :key name)))

(defun list-profiles ()
  "Get a list of all build profiles"
  (let ((profiles '()))
    (maphash (lambda (name profile)
               (declare (ignore name))
               (push profile profiles))
             *build-profiles*)
    (sort profiles #'string< :key #'build-profile-name)))

(defun apply-profile (profile-name)
  "Apply a build profile to the configuration"
  (initialize-config)
  (unless (profile-exists-p profile-name)
    (error 'configuration-error
          :message (format nil "Unknown profile: ~A" profile-name)
          :key profile-name))
  
  (let ((profile (get-profile profile-name))
        (feature-cfg (gethash 'feature-config *config*))
        (package-cfg (gethash 'package-config *config*))
        (build-cfg (gethash 'build-config *config*)))
    
    ;; Apply feature settings
    (loop for (feature value) on (build-profile-features profile) by #'cddr
          when (slot-exists-p feature-cfg feature)
          do (setf (slot-value feature-cfg feature) value))
    
    ;; Apply package groups
    (when (build-profile-package-groups profile)
      (setf (package-config-package-groups package-cfg)
            (build-profile-package-groups profile)))
    
    ;; Set profile in build config
    (setf (build-config-profile build-cfg) profile-name)
    
    t))

;;; ===============================
;;; Helper functions for slot manipulation
;;; ===============================

;; Use a simpler approach that doesn't require MOP
(defmacro do-slots ((slot-var object) &body body)
  "Iterate over all slots of an object without requiring MOP library"
  (let ((obj-var (gensym "object-")))
    `(let ((,obj-var ,object))
       ;; We have to use a hard-coded approach to avoid MOP dependencies
       ;; This will work for SBCL and similar implementations
       (dolist (,slot-var (mapcar #'sb-pcl:slot-definition-name 
                                  (sb-pcl:class-slots (class-of ,obj-var))))
         ,@body))))

;;; ===============================
;;; Register Default Package Groups
;;; ===============================

;; Core functional groups
(register-package-group :base
                       :description "Essential base packages"
                       :packages '("openrc" "nano" "mc" "bash" "parted" "dropbear" "dropbear-ssh" 
                                 "efibootmgr" "e2fsprogs" "e2fsprogs-extra" "dosfstools" "dmraid" 
                                 "fuse" "gawk" "grep" "sed" "util-linux" "wget")
                       :always-include t)

(register-package-group :zfs
                       :description "ZFS filesystem support"
                       :packages '("zfs"))

(register-package-group :btrfs
                       :description "BTRFS filesystem support"
                       :packages '("btrfs-progs"))

(register-package-group :recovery
                       :description "Data recovery tools"
                       :packages '("testdisk" "ddrescue" "rsync" "unzip" "tar"))

(register-package-group :network
                       :description "Network tools and utilities"
                       :packages '("curl" "rsync" "iperf3" "tcpdump" "nftables"))

(register-package-group :crypto
                       :description "Disk encryption and RAID support"
                       :packages '("cryptsetup" "lvm2" "mdadm"))

(register-package-group :tui
                       :description "Text user interface dependencies"
                       :packages '("ncurses-terminfo-base" "less"))

;; Extended functionality groups
(register-package-group :advanced-fs
                       :description "Advanced filesystem support"
                       :packages '("ntfs-3g" "xfsprogs" "gptfdisk" "exfatprogs" "f2fs-tools"))

(register-package-group :disk-diag
                       :description "Disk and hardware diagnostics"
                       :packages '("smartmontools" "hdparm" "nvme-cli" "dmidecode" "lshw"))

(register-package-group :network-diag
                       :description "Network diagnostics and VPN tools"
                       :packages '("ethtool" "nmap" "wireguard-tools" "openvpn"))

(register-package-group :system-tools
                       :description "System monitoring and troubleshooting"
                       :packages '("htop" "strace" "pciutils" "usbutils"))

(register-package-group :data-recovery
                       :description "Advanced data recovery tools"
                       :packages '("testdisk")
                       :conflicts '(:recovery))

(register-package-group :boot-repair
                       :description "Boot repair tools"
                       :packages '("grub"))

(register-package-group :editors
                       :description "Advanced text editors"
                       :packages '("vim" "tmux" "jq"))

(register-package-group :security
                       :description "Security tools"
                       :packages '("openssl"))

;; Special meta groups
(register-package-group :minimal
                       :description "Minimal installation with only essential tools"
                       :conflicts '(:full :standard)
                       :dependencies '(:base))

(register-package-group :standard
                       :description "Standard installation with common tools"
                       :conflicts '(:minimal :full)
                       :dependencies '(:base :zfs :network :crypto :tui))

(register-package-group :full
                       :description "Full installation with all available tools"
                       :conflicts '(:minimal :standard)
                       :dependencies '(:base :zfs :btrfs :recovery :network :crypto :tui
                                     :advanced-fs :disk-diag :network-diag :system-tools 
                                     :boot-repair :editors :security))

;;; ===============================
;;; Register Default Build Profiles
;;; ===============================

(register-build-profile "minimal"
                       :description "Minimal profile optimized for size"
                       :features '(:include-zfs nil
                                  :include-btrfs nil
                                  :include-recovery-tools nil
                                  :include-network-tools nil
                                  :include-crypto nil
                                  :include-tui nil
                                  :include-minimal-kernel t
                                  :include-compression t
                                  :compression-tool "upx"
                                  :include-advanced-fs nil
                                  :include-disk-diag nil
                                  :include-network-diag nil
                                  :include-system-tools nil
                                  :include-data-recovery nil
                                  :include-boot-repair nil
                                  :include-editors nil
                                  :include-security nil
                                  :enable-custom-apks nil)
                       :package-groups '(:base :no-extras)
                       :output-name "OneFileLinux-minimal.efi")

(register-build-profile "standard"
                       :description "Standard profile with balanced features"
                       :features '(:include-zfs t
                                  :include-btrfs nil
                                  :include-recovery-tools t
                                  :include-network-tools t
                                  :include-crypto t
                                  :include-tui t
                                  :include-minimal-kernel nil
                                  :include-compression t
                                  :compression-tool "upx"
                                  :include-advanced-fs nil
                                  :include-disk-diag nil
                                  :include-network-diag nil
                                  :include-system-tools nil
                                  :include-data-recovery nil
                                  :include-boot-repair nil
                                  :include-editors nil
                                  :include-security nil
                                  :enable-custom-apks t)
                       :package-groups '(:base :zfs :network :crypto :tui :recovery)
                       :output-name "OneFileLinux-standard.efi")

(register-build-profile "full"
                       :description "Full profile with all features enabled"
                       :features '(:include-zfs t
                                  :include-btrfs t
                                  :include-recovery-tools t
                                  :include-network-tools t
                                  :include-crypto t
                                  :include-tui t
                                  :include-minimal-kernel nil
                                  :include-compression t
                                  :compression-tool "upx"
                                  :include-advanced-fs t
                                  :include-disk-diag t
                                  :include-network-diag t
                                  :include-system-tools t
                                  :include-data-recovery t
                                  :include-boot-repair t
                                  :include-editors t
                                  :include-security t
                                  :enable-custom-apks t)
                       :package-groups '(:full)
                       :output-name "OneFileLinux-full.efi")

;;; ===============================
;;; Initialize Configuration
;;; ===============================

(initialize-config)

;; Apply default profile
(apply-profile *default-profile*)