;;;; OneFileLinux Test Framework
;;;; Basic testing framework for the OneFileLinux build system

(require :asdf)
(asdf:load-system "onefilelinux")

(defpackage :onefilelinux.test
  (:use :cl :onefilelinux.core)
  (:export #:run-tests
           #:test-core-utilities
           #:test-configuration
           #:test-build-steps))

(in-package :onefilelinux.test)

;;; ===============================
;;; Test Environment Setup
;;; ===============================

(defparameter *test-root* "/tmp/onefilelinux-test"
  "Root directory for test environment")

(defparameter *test-output* (format nil "~A/output" *test-root*)
  "Output directory for tests")

(defparameter *test-config* nil
  "Default configuration for tests")

(defparameter *test-results* '()
  "List of test results")

(defun setup-test-environment ()
  "Set up the test environment"
  (format t "~%Setting up test environment...~%")
  
  ;; Create test directories
  (ensure-directory *test-root*)
  (ensure-directory *test-output*)
  
  ;; Set up logging for tests
  (setf *log-level* :debug)
  (setf *log-file* (format nil "~A/test.log" *test-root*))
  
  ;; Create default test configuration
  (setf *test-config* (onefilelinux.config:make-default-config))
  
  ;; Reset test results
  (setf *test-results* '())
  
  ;; Return success
  t)

(defun teardown-test-environment ()
  "Clean up the test environment"
  (format t "~%Cleaning up test environment...~%")
  
  ;; Remove test directories if requested
  (when (and (boundp '*clean-test-environment*)
             *clean-test-environment*)
    (delete-directory-recursive *test-root*))
  
  ;; Reset test configuration
  (setf *test-config* nil)
  
  ;; Return success
  t)

;;; ===============================
;;; Test Framework
;;; ===============================

(defmacro deftest (name (&key (category :unit) (expected-result t)) &body body)
  "Define a test function"
  `(progn
     (defun ,name ()
       (format t "~%Running test: ~A...~%" ',name)
       (let ((result nil)
             (start-time (get-universal-time))
             (error-message nil))
         (handler-case
             (progn
               ,@body
               (setf result t))
           (error (e)
             (setf error-message (format nil "~A" e))))
         
         (let ((duration (- (get-universal-time) start-time))
               (passed (eq result ,expected-result)))
           
           ;; Record test result
           (push (list ',name :category ,category
                      :passed passed
                      :duration duration
                      :error error-message)
                 *test-results*)
           
           ;; Print test result
           (if passed
               (format t "  PASSED (~A seconds)~%" duration)
               (format t "  FAILED (~A seconds): ~A~%" duration error-message))
           
           ;; Return result
           passed)))
     
     ;; Add test to category
     (pushnew ',name (get-category-tests ,category))))

(defun get-category-tests (category)
  "Get all tests in a category"
  (let ((tests (get category *test-categories* '())))
    (setf (get category *test-categories*) tests)
    tests))

(defparameter *test-categories* (make-hash-table)
  "Hash table of test categories")

(defun run-test-category (category)
  "Run all tests in a category"
  (format t "~%Running ~A tests...~%" category)
  
  (let ((tests (gethash category *test-categories*))
        (passed 0)
        (failed 0))
    
    (dolist (test tests)
      (if (funcall test)
          (incf passed)
          (incf failed)))
    
    (format t "~%~A tests: ~D passed, ~D failed~%" 
           category passed failed)
    
    (values passed failed)))

;;; ===============================
;;; Core Utilities Tests
;;; ===============================

(deftest test-path-utilities (:category :core)
  "Test path manipulation utilities"
  (assert (string= (path-join "/a" "b") "/a/b"))
  (assert (string= (path-join "/a/" "b") "/a/b"))
  (assert (string= (path-join "/a" "/b") "/a/b"))
  (assert (string= (path-join "/a/" "/b") "/a/b"))
  (assert (string= (path-join "/a" "b" "c") "/a/b/c"))
  (assert (string= (normalize-path "/a/./b/../c") "/a/c")))

(deftest test-string-utilities (:category :core)
  "Test string manipulation utilities"
  (assert (string-prefix-p "abc" "abcdef"))
  (assert (not (string-prefix-p "def" "abcdef")))
  (assert (string-suffix-p "def" "abcdef"))
  (assert (not (string-suffix-p "abc" "abcdef")))
  (assert (equal (split-string "a,b,c" ",") '("a" "b" "c")))
  (assert (string= (string-join '("a" "b" "c") ",") "a,b,c")))

(deftest test-file-utilities (:category :core)
  "Test file system utilities"
  
  ;; Create a temporary test file
  (let ((test-file (format nil "~A/test-file.txt" *test-root*))
        (test-content "This is a test file."))
    
    ;; Write test content to file
    (write-string-to-file test-content test-file)
    
    ;; Test file exists
    (assert (file-exists-p test-file))
    
    ;; Test file content
    (assert (string= (read-file-as-string test-file) test-content))
    
    ;; Test file size
    (assert (= (file-size test-file) (length test-content)))
    
    ;; Test file exists in directory
    (assert (member (file-namestring test-file) 
                   (directory-files *test-root*)
                   :test #'string=))))

;;; ===============================
;;; Configuration Tests
;;; ===============================

(deftest test-config-basics (:category :config)
  "Test basic configuration functionality"
  (let ((config (onefilelinux.config:make-default-config)))
    ;; Test config is a hash table
    (assert (hash-table-p config))
    
    ;; Test config values can be set and retrieved
    (setf (onefilelinux.config:config-value config :test :key1) "value1")
    (assert (string= (onefilelinux.config:config-value config :test :key1) "value1"))
    
    ;; Test default values
    (assert (string= (onefilelinux.config:config-value config :test :nonexistent "default") 
                    "default"))))

;;; ===============================
;;; Build Step Tests
;;; ===============================

(deftest test-build-context (:category :build)
  "Test build context creation and accessors"
  ;; Create a build context
  (let ((context (onefilelinux.build:make-build-context
                 :config *test-config*
                 :working-dir *test-root*
                 :output-dir *test-output*)))
    
    ;; Test context slots
    (assert (onefilelinux.build:build-context-p context))
    (assert (eq (onefilelinux.build:build-context-config context) *test-config*))
    (assert (string= (onefilelinux.build:build-context-working-dir context) *test-root*))
    (assert (string= (onefilelinux.build:build-context-output-dir context) *test-output*))))

;;; ===============================
;;; Main Test Runner
;;; ===============================

(deftest test-dry-run-basic (:category :features)
  "Test basic dry run functionality"
  (let* ((working-dir (uiop:getcwd))
         (output-dir (ensure-directory 
                      (path-join working-dir "output")))
         (context (onefilelinux.build:make-build-context
                   :config onefilelinux.config:*config*
                   :working-dir working-dir
                   :output-dir output-dir
                   :dry-run t)))
    
    ;; Test dry run flag is set
    (assert (onefilelinux.build:build-context-dry-run context))
    
    ;; Test dry run until is nil
    (assert (null (onefilelinux.build:build-context-dry-run-until context)))))

(deftest test-dry-run-until (:category :features)
  "Test dry run until specific step"
  (let* ((working-dir (uiop:getcwd))
         (output-dir (ensure-directory 
                      (path-join working-dir "output")))
         (context (onefilelinux.build:make-build-context
                   :config onefilelinux.config:*config*
                   :working-dir working-dir
                   :output-dir output-dir
                   :dry-run t
                   :dry-run-until :prepare)))
    
    ;; Test dry run flag is set
    (assert (onefilelinux.build:build-context-dry-run context))
    
    ;; Test dry run until is set correctly
    (assert (eq (onefilelinux.build:build-context-dry-run-until context) :prepare))))

(defun run-tests ()
  "Run all tests"
  (format t "~%Running OneFileLinux tests...~%")
  
  ;; Set up test environment
  (setup-test-environment)
  
  ;; Run tests by category
  (let ((total-passed 0)
        (total-failed 0))
    
    ;; Run each category
    (dolist (category '(:core :config :build :features))
      (multiple-value-bind (passed failed)
          (run-test-category category)
        (incf total-passed passed)
        (incf total-failed failed)))
    
    ;; Print summary
    (format t "~%Test summary: ~D passed, ~D failed~%" 
           total-passed total-failed)
    
    ;; Clean up test environment
    (teardown-test-environment)
    
    ;; Return status code
    (if (zerop total-failed)
        (progn
          (format t "~%All tests passed!~%")
          (values t total-passed total-failed))
        (progn
          (format t "~%Some tests failed!~%")
          (values nil total-passed total-failed)))))

;; Run tests if invoked as script
(eval-when (:execute)
  (let ((result (run-tests)))
    (uiop:quit (if result 0 1))))