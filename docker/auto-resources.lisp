;;;; OneFileLinux Docker Resource Management
;;;; Utilities for detecting and managing system resources in Docker environment
(defpackage :onefilelinux.docker.resources
  (:use :cl :onefilelinux.core)
  (:export #:detect-resources
           #:allocate-resources
           #:with-resource-limits
           #:resource-usage-report
           #:scale-resources-to-workload))

(in-package :onefilelinux.docker.resources)

;;; ----------------------------------------------------
;;; Resource Detection
;;; ----------------------------------------------------

(defclass system-resources ()
  ((cpu-count :initarg :cpu-count
              :accessor resource-cpu-count
              :documentation "Number of available CPU cores")
   (memory-mb :initarg :memory-mb
              :accessor resource-memory-mb
              :documentation "Available memory in megabytes")
   (disk-mb :initarg :disk-mb
            :accessor resource-disk-mb
            :documentation "Available disk space in megabytes")
   (cpu-limit :initarg :cpu-limit
              :accessor resource-cpu-limit
              :initform nil
              :documentation "CPU limit, if constrained")
   (memory-limit :initarg :memory-limit
                :accessor resource-memory-limit
                :initform nil
                :documentation "Memory limit, if constrained"))
  (:documentation "Container for system resource information."))

(defmethod print-object ((obj system-resources) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "CPU: ~A cores, Memory: ~A MB, Disk: ~A MB"
            (resource-cpu-count obj)
            (resource-memory-mb obj)
            (resource-disk-mb obj))
    (when (resource-cpu-limit obj)
      (format stream ", CPU Limit: ~A" (resource-cpu-limit obj)))
    (when (resource-memory-limit obj)
      (format stream ", Memory Limit: ~A MB" (resource-memory-limit obj)))))

(defun detect-resources ()
  "Detect available system resources in the Docker environment."
  (log-message :info "Detecting system resources...")
  
  (let ((cpu-count (detect-cpu-count))
        (memory-mb (detect-memory-mb))
        (disk-mb (detect-disk-mb))
        (cpu-limit (detect-cpu-limit))
        (memory-limit (detect-memory-limit)))
    
    (make-instance 'system-resources
                  :cpu-count cpu-count
                  :memory-mb memory-mb
                  :disk-mb disk-mb
                  :cpu-limit cpu-limit
                  :memory-limit memory-limit)))

(defun detect-cpu-count ()
  "Detect the number of available CPU cores."
  (let ((result (run-command-output "nproc" '())))
    (parse-integer (string-trim '(#\Space #\Tab #\Newline) result))))

(defun detect-memory-mb ()
  "Detect the amount of available memory in MB."
  (let* ((mem-info (run-command-output "cat" '("/proc/meminfo")))
         (total-line (find-if (lambda (line)
                               (prefixp "MemTotal:" line))
                             (split-string mem-info #\Newline)))
         (mem-kb (parse-integer (regex-replace-all "[^0-9]" total-line ""))))
    (floor mem-kb 1024)))

(defun detect-disk-mb ()
  "Detect the amount of available disk space in MB."
  (let* ((df-output (run-command-output "df" '("-m" ".")))
         (lines (split-string df-output #\Newline))
         (data-line (second lines))
         (parts (split-string data-line))
         (available (nth 3 parts)))
    (parse-integer available)))

(defun detect-cpu-limit ()
  "Detect CPU limit in the container, if any."
  (let ((cgroup-path "/sys/fs/cgroup/cpu/cpu.cfs_quota_us"))
    (when (file-exists-p cgroup-path)
      (let* ((quota (parse-integer (string-trim '(#\Space #\Tab #\Newline) 
                                              (read-file-as-string cgroup-path))))
             (period-path "/sys/fs/cgroup/cpu/cpu.cfs_period_us")
             (period (parse-integer (string-trim '(#\Space #\Tab #\Newline)
                                               (read-file-as-string period-path)))))
        (when (> quota 0)
          (/ quota period))))))

(defun detect-memory-limit ()
  "Detect memory limit in the container, if any."
  (let ((cgroup-path "/sys/fs/cgroup/memory/memory.limit_in_bytes"))
    (when (file-exists-p cgroup-path)
      (let* ((limit-bytes (parse-integer (string-trim '(#\Space #\Tab #\Newline)
                                                     (read-file-as-string cgroup-path))))
             (host-mem (detect-memory-mb)))
        (when (< limit-bytes (* host-mem 1024 1024))
          (floor limit-bytes (* 1024 1024)))))))

;;; ----------------------------------------------------
;;; Resource Allocation
;;; ----------------------------------------------------

(defun allocate-resources (resources &key (workload-scale 1.0))
  "Allocate resources based on detection and workload scale."
  (log-message :info "Allocating resources for workload (scale: ~,2F)..." workload-scale)
  
  ;; Calculate CPU allocation
  (let* ((cpu-available (resource-cpu-count resources))
         (mem-available (resource-memory-mb resources))
         (cpu-limit (resource-cpu-limit resources))
         (mem-limit (resource-memory-limit resources))
         
         ;; Apply limits if present
         (effective-cpu (if cpu-limit (min cpu-available cpu-limit) cpu-available))
         (effective-mem (if mem-limit (min mem-available mem-limit) mem-available))
         
         ;; Apply workload scaling
         (cpu-allocated (max 1 (floor (* effective-cpu workload-scale))))
         (mem-allocated (floor (* effective-mem workload-scale))))
    
    (log-message :debug "Resource allocation: ~A CPUs, ~A MB memory" 
                cpu-allocated mem-allocated)
    
    (list :cpu cpu-allocated :memory mem-allocated)))

(defun scale-resources-to-workload (resources profile)
  "Scale resource allocation based on the build profile."
  (let ((workload-scale (case (intern (string-upcase profile) :keyword)
                          (:minimal 0.5)
                          (:standard 0.75)
                          (:full 1.0)
                          (otherwise 0.75))))
    
    (allocate-resources resources :workload-scale workload-scale)))

;;; ----------------------------------------------------
;;; Resource Limiting
;;; ----------------------------------------------------

(defmacro with-resource-limits ((allocation) &body body)
  "Execute body with resource limits applied."
  `(let ((cpu-count (getf ,allocation :cpu))
         (memory-mb (getf ,allocation :memory)))
     
     (log-message :debug "Applying resource limits: ~A CPUs, ~A MB memory" 
                 cpu-count memory-mb)
     
     ;; Set environment variables to influence child processes
     (let ((old-env (copy-environment)))
       (unwind-protect
           (progn
             ;; Set process resource limits
             (setenv "MAKEFLAGS" (format nil "-j~D" cpu-count))
             (setenv "MALLOC_ARENA_MAX" "2")  ; Reduce memory fragmentation
             
             ;; Execute the body with resource limits
             ,@body)
         
         ;; Restore original environment
         (restore-environment old-env)))))

(defun copy-environment ()
  "Create a copy of the current environment variables."
  (mapcar (lambda (var)
            (let ((pos (position #\= var)))
              (when pos
                (cons (subseq var 0 pos) (subseq var (1+ pos))))))
          (run-command-output "env" '())))

(defun restore-environment (env-vars)
  "Restore environment variables from saved state."
  (dolist (var env-vars)
    (setenv (car var) (cdr var))))

(defun setenv (name value)
  "Set an environment variable."
  (let ((command (format nil "export ~A=\"~A\"" name value)))
    (run-command "sh" (list "-c" command))))

;;; ----------------------------------------------------
;;; Resource Monitoring
;;; ----------------------------------------------------

(defun start-resource-monitoring ()
  "Start monitoring resource usage."
  (log-message :debug "Starting resource usage monitoring")
  
  ;; In a real implementation, this would start a background thread
  ;; Here we just return a placeholder
  (list :start-time (get-universal-time)
        :sampling-interval 5))

(defun stop-resource-monitoring (monitor)
  "Stop resource usage monitoring and return usage report."
  (log-message :debug "Stopping resource usage monitoring")
  
  (let ((runtime (- (get-universal-time) (getf monitor :start-time))))
    (resource-usage-report runtime)))

(defun resource-usage-report (runtime)
  "Generate a resource usage report."
  (let* ((cpu-usage (get-cpu-usage))
         (memory-usage (get-memory-usage))
         (disk-usage (get-disk-usage)))
    
    (log-message :info "Resource usage report:")
    (log-message :info "  Runtime: ~A seconds" runtime)
    (log-message :info "  CPU usage average: ~,2F%" cpu-usage)
    (log-message :info "  Memory peak: ~A MB" memory-usage)
    (log-message :info "  Disk usage: ~A MB" disk-usage)
    
    (list :runtime runtime
          :cpu-usage cpu-usage
          :memory-usage memory-usage
          :disk-usage disk-usage)))

(defun get-cpu-usage ()
  "Get the current CPU usage percentage."
  (let* ((stat1 (read-proc-stat))
         (sleep-time 0.5)
         (stat2 (progn (sleep sleep-time) (read-proc-stat)))
         (user1 (getf stat1 :user))
         (system1 (getf stat1 :system))
         (idle1 (getf stat1 :idle))
         (total1 (+ user1 system1 idle1))
         (user2 (getf stat2 :user))
         (system2 (getf stat2 :system))
         (idle2 (getf stat2 :idle))
         (total2 (+ user2 system2 idle2))
         (total-diff (- total2 total1))
         (idle-diff (- idle2 idle1)))
    
    ;; Calculate percentage of CPU used
    (if (> total-diff 0)
        (* 100 (/ (- total-diff idle-diff) total-diff))
        0.0)))

(defun read-proc-stat ()
  "Read CPU statistics from /proc/stat."
  (let* ((stat (read-file-as-string "/proc/stat"))
         (lines (split-string stat #\Newline))
         (cpu-line (find-if (lambda (line) (prefixp "cpu " line)) lines))
         (parts (split-string cpu-line)))
    
    (list :user (parse-integer (nth 1 parts))
          :nice (parse-integer (nth 2 parts))
          :system (parse-integer (nth 3 parts))
          :idle (parse-integer (nth 4 parts))
          :iowait (parse-integer (nth 5 parts)))))

(defun get-memory-usage ()
  "Get the current memory usage in MB."
  (let* ((status (read-file-as-string "/proc/self/status"))
         (lines (split-string status #\Newline))
         (vmrss-line (find-if (lambda (line) (prefixp "VmRSS:" line)) lines))
         (parts (split-string vmrss-line))
         (kb (parse-integer (second parts))))
    (floor kb 1024)))

(defun get-disk-usage ()
  "Get the current disk usage of the build directory in MB."
  (let* ((output (run-command-output "du" '("-sm" ".")))
         (parts (split-string output))
         (mb (parse-integer (first parts))))
    mb))

;;; ----------------------------------------------------
;;; Utility Functions
;;; ----------------------------------------------------

(defun prefixp (prefix string)
  "Check if string starts with prefix."
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))