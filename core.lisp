;;;; OneFileLinux Core Utilities
;;;; Foundational utilities and common functions for the OneFileLinux build system

(defpackage :onefilelinux.core
  (:use :cl)
  (:export
   ;; Logging functions
   #:log-message
   #:with-logging-context
   #:*log-level*
   #:*log-file*
   #:*log-timestamp*
   
   ;; Command execution
   #:run-command
   #:run-command-status
   #:with-environment-variables
   #:shell-command-to-string
   #:which
   
   ;; File system operations
   #:file-exists-p
   #:directory-exists-p
   #:ensure-directory
   #:file-size
   #:file-mtime
   #:read-file-content
   #:write-file-content
   #:temp-file-name
   #:with-temporary-file
   
   ;; Error handling
   #:onefilelinux-error
   #:error-message
   #:error-context
   #:command-error
   #:command-error-command
   #:command-error-exit-code
   #:command-error-output
   #:configuration-error
   #:configuration-error-key
   #:file-system-error
   #:file-system-error-path
   #:with-error-handling
   #:report-error
   
   ;; String/path utilities
   #:split-string
   #:string-join
   #:string-prefix-p
   #:string-suffix-p
   #:normalize-path
   #:path-join
   
   ;; Environment management
   #:getenv
   #:setenv
   #:with-environment
   
   ;; Resource management
   #:with-resource
   #:with-cleanup
   
   ;; System utilities
   #:detect-os
   #:detect-cpus
   #:detect-memory
   #:detect-disk-space))

(in-package :onefilelinux.core)

;;; ===============================
;;; Configuration and global state
;;; ===============================

(defparameter *log-level* :info
  "Current logging level. One of :debug, :info, :warning, :error, :critical")

(defparameter *log-file* nil
  "File to write logs to. If nil, only writes to standard output.")

(defparameter *log-timestamp* t
  "Whether to include timestamps in log messages.")

(defparameter *log-context* nil
  "Dynamic context for logging (added to all messages)")

(defparameter *colors*
  '(:debug     "[0;36m"  ; Cyan
    :info      "[0;34m"  ; Blue
    :success   "[0;32m"  ; Green
    :warning   "[0;33m"  ; Yellow
    :error     "[0;31m"  ; Red
    :critical  "[1;31m"  ; Bright Red
    :reset     "[0m")
  "ANSI color codes for different message types")

;;; ===============================
;;; Error condition hierarchy
;;; ===============================

(define-condition onefilelinux-error (error)
  ((message :initarg :message :reader error-message :type string)
   (context :initarg :context :reader error-context :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A~@[ [~A]~]" 
                     (error-message condition)
                     (error-context condition))))
  (:documentation "Base error for all OneFileLinux errors"))

(define-condition command-error (onefilelinux-error)
  ((command :initarg :command :reader command-error-command)
   (exit-code :initarg :exit-code :reader command-error-exit-code)
   (output :initarg :output :reader command-error-output))
  (:report (lambda (condition stream)
             (format stream "Command failed with status ~A: ~A~@[ [~A]~]~@[~%Output: ~A~]" 
                     (command-error-exit-code condition)
                     (command-error-command condition)
                     (error-context condition)
                     (command-error-output condition))))
  (:documentation "Error caused by a shell command failure"))

(define-condition configuration-error (onefilelinux-error)
  ((key :initarg :key :reader configuration-error-key :initform nil))
  (:report (lambda (condition stream)
             (format stream "Configuration error~@[ for key '~A'~]: ~A~@[ [~A]~]" 
                     (configuration-error-key condition)
                     (error-message condition)
                     (error-context condition))))
  (:documentation "Error caused by invalid configuration"))

(define-condition file-system-error (onefilelinux-error)
  ((path :initarg :path :reader file-system-error-path))
  (:report (lambda (condition stream)
             (format stream "File system error for path '~A': ~A~@[ [~A]~]" 
                     (file-system-error-path condition)
                     (error-message condition)
                     (error-context condition))))
  (:documentation "Error caused by file system operations"))

;;; ===============================
;;; Logging system implementation
;;; ===============================

(defun log-level-priority (level)
  "Return the numeric priority of a log level"
  (case level
    (:debug 10)
    (:info 20)
    (:success 25)
    (:warning 30)
    (:error 40)
    (:critical 50)
    (otherwise 0)))

(defun format-timestamp ()
  "Return a formatted timestamp for log messages"
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" 
            year month date hour minute second)))

(defun format-log-message (level message &optional color-enabled)
  "Format a log message with appropriate prefixes and colors"
  (let* ((level-str (case level
                      (:debug    "[DEBUG] ")
                      (:info     "[INFO] ")
                      (:success  "[SUCCESS] ")
                      (:warning  "[WARNING] ")
                      (:error    "[ERROR] ")
                      (:critical "[CRITICAL] ")
                      (otherwise (format nil "[~A] " level))))
         (timestamp (when *log-timestamp* (format nil "~A " (format-timestamp))))
         (context (when *log-context* (format nil "[~A] " *log-context*)))
         (color (when color-enabled (getf *colors* level (getf *colors* :reset))))
         (reset (when color-enabled (getf *colors* :reset))))
    
    (format nil "~@[~A~]~@[~A~]~@[~A~]~A~@[~A~]"
            timestamp
            context
            color 
            (format nil "~A~A" level-str message)
            reset)))

(defun log-message (level format-string &rest args)
  "Log a message with the given level and format arguments"
  (when (>= (log-level-priority level) (log-level-priority *log-level*))
    (let ((message (apply #'format nil format-string args)))
      ;; Log to standard output with colors
      (format t "~A~%" (format-log-message level message t))
      
      ;; Log to file if configured (without colors)
      (when *log-file*
        (with-open-file (stream *log-file* 
                               :direction :output 
                               :if-exists :append 
                               :if-does-not-exist :create)
          (format stream "~A~%" (format-log-message level message nil)))))))

(defmacro with-logging-context ((context) &body body)
  "Execute body with the specified logging context"
  `(let ((*log-context* ,context))
     ,@body))

;;; ===============================
;;; Command execution utilities
;;; ===============================

(defun run-command (command &key capture-error ignore-error)
  "Run a shell command and return its output and exit status.
   If capture-error is true, stderr is included in the output.
   If ignore-error is true, no error is signaled for non-zero exit codes."
  (let ((output-redirect (if capture-error 
                             "2>&1" 
                             "2>/dev/null"))
        (cmd (format nil "set -o pipefail; ~A" command)))
    (handler-case
        (multiple-value-bind (output exit-code)
            (uiop:run-program (format nil "bash -c \"~A ~A\"" cmd output-redirect)
                             :output :string
                             :ignore-error-status t)
          (when (and (not ignore-error) 
                    (not (zerop exit-code)))
            (error 'command-error 
                  :message (format nil "Command failed with status ~A" exit-code)
                  :command command 
                  :exit-code exit-code
                  :output output))
          (values output exit-code))
      (error (e)
        (if ignore-error 
            (values nil -1)
            (error 'command-error 
                  :message (format nil "Command execution error: ~A" e)
                  :command command
                  :exit-code -1
                  :output nil))))))

(defun run-command-status (command)
  "Run a command and return t if it succeeded, nil otherwise"
  (multiple-value-bind (output exit-code)
      (run-command command :ignore-error t)
    (zerop exit-code)))

(defun shell-command-to-string (command)
  "Run a command and return its output as a string with whitespace trimmed"
  (string-trim '(#\Newline #\Return #\Space #\Tab)
              (run-command command :ignore-error t)))

(defun which (command)
  "Check if a command exists in the PATH"
  (run-command-status (format nil "which ~A > /dev/null 2>&1" command)))

(defmacro with-environment-variables (bindings &body body)
  "Execute body with environment variables temporarily set"
  (let ((old-values (gensym "old-values-")))
    `(let ((,old-values '()))
       ;; Save old values and set new ones
       (dolist (binding ',bindings)
         (destructuring-bind (var value) binding
           (let ((old-value (uiop:getenv (string var))))
             (push (cons (string var) old-value) ,old-values)
             (setf (uiop:getenv (string var)) ,value))))
       
       ;; Execute body with cleanup
       (unwind-protect
            (progn ,@body)
         ;; Restore old values
         (dolist (pair ,old-values)
           (setf (uiop:getenv (car pair)) (cdr pair)))))))

;;; ===============================
;;; File system operations
;;; ===============================

(defun file-exists-p (path)
  "Check if a file exists"
  (probe-file path))

(defun directory-exists-p (path)
  "Check if a directory exists"
  (and (probe-file path)
       (uiop:directory-exists-p path)))

(defun ensure-directory (path)
  "Create directory if it doesn't exist"
  (ensure-directories-exist
   (if (and (stringp path) (not (uiop:string-suffix-p path "/")))
       (concatenate 'string path "/")
       path)))

(defun file-size (path)
  "Get the size of a file in bytes"
  (if (file-exists-p path)
      (with-open-file (stream path :direction :input 
                             :element-type '(unsigned-byte 8))
        (file-length stream))
      (error 'file-system-error 
            :message "File does not exist" 
            :path path)))

(defun file-mtime (path)
  "Get the modification time of a file as universal time"
  (if (file-exists-p path)
      (file-write-date path)
      (error 'file-system-error 
            :message "File does not exist" 
            :path path)))

(defun read-file-content (path &key (encoding :utf-8))
  "Read the entire content of a file as a string"
  (if (file-exists-p path)
      (uiop:read-file-string path :external-format encoding)
      (error 'file-system-error 
            :message "File does not exist" 
            :path path)))

(defun write-file-content (path content &key (encoding :utf-8) (if-exists :supersede))
  "Write content to a file"
  (ensure-directory (directory-namestring path))
  (with-open-file (out path
                      :direction :output
                      :external-format encoding
                      :if-exists if-exists
                      :if-does-not-exist :create)
    (write-string content out)))

(defun temp-file-name (&optional prefix suffix)
  "Generate a temporary file name"
  (let ((prefix (or prefix "onefilelinux-"))
        (suffix (or suffix "")))
    (format nil "/tmp/~A~36R~A" 
            prefix 
            (random (expt 36 8))
            suffix)))

(defmacro with-temporary-file ((var &key (prefix "onefilelinux-") (suffix "")) &body body)
  "Execute body with var bound to a temporary file path, cleaning up after"
  (let ((tempfile (gensym "tempfile-")))
    `(let* ((,tempfile (temp-file-name ,prefix ,suffix))
            (,var ,tempfile))
       (unwind-protect
            (progn ,@body)
         (when (file-exists-p ,tempfile)
           (ignore-errors (delete-file ,tempfile)))))))

;;; ===============================
;;; Error handling
;;; ===============================

(defmacro with-error-handling ((error-var &key on-error) &body body)
  "Execute body, capturing and optionally handling any errors"
  `(handler-case
       (progn ,@body)
     (onefilelinux-error (,error-var)
       ,(if on-error
            `(funcall ,on-error ,error-var)
            `(progn
               (report-error ,error-var)
               nil)))
     (error (,error-var)
       ,(if on-error
            `(funcall ,on-error (make-condition 'onefilelinux-error
                                               :message (format nil "Unexpected error: ~A" ,error-var)))
            `(progn
               (log-message :error "Unexpected error: ~A" ,error-var)
               nil)))))

(defun report-error (condition)
  "Report an error condition to the log"
  (typecase condition
    (command-error
     (log-message :error "Command failed: ~A (status ~A)~@[ [~A]~]"
                 (command-error-command condition)
                 (command-error-exit-code condition)
                 (error-context condition)))
    (configuration-error
     (log-message :error "Configuration error~@[ for '~A'~]: ~A~@[ [~A]~]"
                 (when (slot-boundp condition 'key) 
                   (configuration-error-key condition))
                 (error-message condition)
                 (error-context condition)))
    (file-system-error
     (log-message :error "File system error for '~A': ~A~@[ [~A]~]"
                 (file-system-error-path condition)
                 (error-message condition)
                 (error-context condition)))
    (onefilelinux-error
     (log-message :error "~A~@[ [~A]~]" 
                 (error-message condition)
                 (error-context condition)))
    (t
     (log-message :error "Error: ~A" condition))))

;;; ===============================
;;; String and path utilities 
;;; ===============================

(defun split-string (string &optional (delimiter " ") (trim t))
  "Split a string by a delimiter"
  (let ((result '())
        (start 0)
        (delim-len (length delimiter)))
    (loop
      (let ((pos (search delimiter string :start2 start)))
        (if pos
            (progn
              (push (if trim
                        (string-trim '(#\Space #\Tab #\Newline) 
                                    (subseq string start pos))
                        (subseq string start pos))
                    result)
              (setf start (+ pos delim-len)))
            (progn
              (push (if trim
                        (string-trim '(#\Space #\Tab #\Newline) 
                                    (subseq string start))
                        (subseq string start))
                    result)
              (return)))))
    (nreverse result)))

(defun string-join (strings &optional (delimiter " "))
  "Join a list of strings with a delimiter"
  (with-output-to-string (s)
    (let ((first t))
      (dolist (str strings)
        (if first
            (setf first nil)
            (write-string delimiter s))
        (write-string str s)))))

(defun string-prefix-p (prefix string)
  "Check if string starts with prefix"
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun string-suffix-p (suffix string)
  "Check if string ends with suffix"
  (and (>= (length string) (length suffix))
       (string= suffix string :start2 (- (length string) (length suffix)))))

(defun normalize-path (path)
  "Normalize a path, removing redundant elements"
  (let* ((path-components (split-string path "/"))
         (result '()))
    (dolist (component path-components)
      (cond
        ((string= component "") nil) ; Skip empty components
        ((string= component ".") nil) ; Skip current directory
        ((string= component "..") ; Handle parent directory
         (if result
             (pop result)
             (push ".." result)))
        (t (push component result))))
    
    ;; Construct the normalized path
    (let ((result-path (string-join (nreverse result) "/")))
      (if (char= (char path 0) #\/)
          (concatenate 'string "/" result-path)
          result-path))))

(defun path-join (&rest paths)
  "Join path components"
  (if (null paths)
      ""
      (let ((result (car paths)))
        (dolist (path (cdr paths))
          (if (and (not (string= result ""))
                  (not (char= (char result (1- (length result))) #\/))
                  (not (char= (char path 0) #\/)))
              (setf result (concatenate 'string result "/" path))
              (setf result (concatenate 'string result path))))
        (normalize-path result))))

;;; ===============================
;;; Environment management
;;; ===============================

(defun getenv (var &optional default)
  "Get an environment variable, optionally with a default value"
  (or (uiop:getenv var) default))

(defun setenv (var value)
  "Set an environment variable"
  (setf (uiop:getenv var) value))

(defmacro with-environment (bindings &body body)
  "Execute body with environment variables set"
  `(with-environment-variables ,bindings ,@body))

;;; ===============================
;;; Resource management
;;; ===============================

(defmacro with-resource ((var init cleanup) &body body)
  "Manage a resource with proper cleanup"
  `(let ((,var ,init))
     (unwind-protect
          (progn ,@body)
       ,cleanup)))

(defmacro with-cleanup (cleanup &body body)
  "Execute body with cleanup form run afterward"
  `(unwind-protect
        (progn ,@body)
     ,cleanup))

;;; ===============================
;;; System information utilities
;;; ===============================

(defun detect-os ()
  "Detect the operating system and version"
  (cond
    ;; Linux with os-release file
    ((probe-file "/etc/os-release")
     (let* ((os-release (read-file-content "/etc/os-release"))
            (name-pos (search "NAME=" os-release))
            (version-pos (search "VERSION_ID=" os-release))
            (os-name (when name-pos
                      (let* ((start (+ name-pos 5))
                             (end (position #\Newline os-release :start start))
                             (value (subseq os-release start end)))
                        (string-trim '(#\" #\Space) value))))
            (os-version (when version-pos
                         (let* ((start (+ version-pos 11))
                                (end (position #\Newline os-release :start start))
                                (value (subseq os-release start end)))
                           (string-trim '(#\" #\Space) value)))))
       (values os-name os-version "linux")))
    
    ;; macOS
    ((run-command-status "uname | grep -q Darwin")
     (let ((version (shell-command-to-string "sw_vers -productVersion")))
       (values "macOS" version "darwin")))
    
    ;; Windows (for completeness)
    ((run-command-status "uname | grep -q MINGW")
     (values "Windows" 
            (shell-command-to-string "cmd /c ver | findstr /r \"[0-9][0-9]*\\.[0-9][0-9]*\\.[0-9][0-9]*\"")
            "windows"))
    
    ;; Generic Unix
    ((run-command-status "uname")
     (let ((uname (shell-command-to-string "uname -a")))
       (values "Unix" uname "unix")))
    
    ;; Unknown
    (t (values "Unknown" "Unknown" "unknown"))))

(defun detect-cpus ()
  "Detect the number of CPU cores"
  (multiple-value-bind (os-name os-version os-type) (detect-os)
    (declare (ignore os-name os-version))
    (cond
      ((string= os-type "darwin")
       (parse-integer (shell-command-to-string "sysctl -n hw.ncpu")))
      
      ((string= os-type "linux")
       (parse-integer (shell-command-to-string "nproc")))
      
      ((string= os-type "windows")
       (parse-integer (shell-command-to-string "echo %NUMBER_OF_PROCESSORS%")))
      
      (t 1))))

(defun detect-memory ()
  "Detect the system memory in bytes"
  (multiple-value-bind (os-name os-version os-type) (detect-os)
    (declare (ignore os-name os-version))
    (cond
      ((string= os-type "darwin")
       (parse-integer (shell-command-to-string "sysctl -n hw.memsize")))
      
      ((string= os-type "linux")
       (let ((mem-kb (parse-integer 
                      (shell-command-to-string 
                       "grep MemTotal /proc/meminfo | awk '{print $2}'"))))
         (* mem-kb 1024)))
      
      ((string= os-type "windows")
       ;; Best effort on Windows (might not work in all environments)
       (parse-integer 
        (shell-command-to-string 
         "wmic ComputerSystem get TotalPhysicalMemory | findstr /r \"[0-9]\"")))
      
      (t (* 2 1024 1024 1024))))) ; Default to 2GB

(defun detect-disk-space (path)
  "Detect available disk space at path in bytes"
  (multiple-value-bind (os-name os-version os-type) (detect-os)
    (declare (ignore os-name os-version))
    (cond
      ((or (string= os-type "darwin") (string= os-type "linux"))
       (parse-integer 
        (shell-command-to-string 
         (format nil "df -k ~A | tail -1 | awk '{print $4}'" path)))
       (* 1024)) ; Convert KB to bytes
      
      ((string= os-type "windows")
       ;; Best effort on Windows (might not work in all environments)
       (parse-integer 
        (shell-command-to-string 
         (format nil "wmic LogicalDisk Where \"DeviceID='~A:'\" Get FreeSpace | findstr /r \"[0-9]\"" 
                (char path 0)))))
      
      (t (* 10 1024 1024 1024))))) ; Default to 10GB

;;; ===============================
;;; Additional Utility Functions
;;; ===============================

(defun copy-directory-recursive (src dst)
  "Copy directory recursively"
  (log-message :debug "Copying directory recursively: ~A -> ~A" src dst)
  (ensure-directory dst)
  (run-command (format nil "cp -R ~A/. ~A/" src dst)))

(defun delete-directory-recursively (path)
  "Delete directory recursively"
  (log-message :debug "Deleting directory recursively: ~A" path)
  (run-command (format nil "rm -rf ~A" path)))

(defun regex-replace (pattern string replacement)
  "Replace pattern in string with replacement"
  (cl-ppcre:regex-replace pattern string replacement))

(defun regex-replace-all (pattern string replacement)
  "Replace all occurrences of pattern in string with replacement"
  (cl-ppcre:regex-replace-all pattern string replacement))

(defun chdir (directory)
  "Change the current working directory"
  (log-message :debug "Changing directory to: ~A" directory)
  (if (directory-exists-p directory)
      (uiop:chdir directory)
      (error 'file-system-error
             :message "Cannot change directory - directory does not exist"
             :path directory)))

(defun getcwd ()
  "Get the current working directory"
  (uiop:getcwd))

(defun chmod (path mode)
  "Change file permissions"
  (log-message :debug "Changing permissions of ~A to ~O" path mode)
  (run-command (format nil "chmod ~O ~A" mode path)))

(defun write-string-to-file (string path &key (if-exists :supersede))
  "Write a string to a file"
  (log-message :debug "Writing string to file: ~A" path)
  (ensure-directory (directory-namestring path))
  (with-open-file (out path
                      :direction :output
                      :if-exists if-exists
                      :if-does-not-exist :create)
    (write-string string out)))

(defun read-file-as-string (path)
  "Read a file as a string"
  (log-message :debug "Reading file as string: ~A" path)
  (if (file-exists-p path)
      (with-open-file (in path :direction :input)
        (let ((string (make-string (file-length in))))
          (read-sequence string in)
          string))
      (error 'file-system-error
             :message "File does not exist"
             :path path)))

(defun file-directory-p (path)
  "Check if path is a directory"
  (and (probe-file path)
       (uiop:directory-exists-p path)))

(defun directory-files (path)
  "Get a list of files in a directory"
  (log-message :debug "Listing files in directory: ~A" path)
  (if (directory-exists-p path)
      (mapcar #'file-namestring 
              (uiop:directory-files path))
      (error 'file-system-error
             :message "Directory does not exist"
             :path path)))

(defun detect-memory-mb ()
  "Detect system memory in megabytes"
  (floor (detect-memory) 1048576))