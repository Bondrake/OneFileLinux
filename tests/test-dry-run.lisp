;;;; OneFileLinux Dry Run Test Script
;;;; Tests the dry run functionality

(require :asdf)
(asdf:load-system "onefilelinux")

(defpackage :onefilelinux.tests.dry-run
  (:use :cl :onefilelinux.core :onefilelinux.build :onefilelinux.config)
  (:export #:run-tests))

(in-package :onefilelinux.tests.dry-run)

(defun test-dry-run-basic ()
  "Test basic dry run functionality"
  (format t "~%Test: Basic Dry Run~%")
  (format t "~70,,,'-A~%" "")

  ;; Create build context with dry run enabled
  (let* ((working-dir (uiop:getcwd))
         (output-dir (ensure-directory 
                      (path-join working-dir "output")))
         (context (make-build-context
                   :config *config*
                   :working-dir working-dir
                   :output-dir output-dir
                   :verbose t
                   :dry-run t)))
    
    ;; Run the build with dry run enabled
    (format t "Running build with dry-run=T~%")
    (let ((result (run-build context)))
      (format t "Build result: ~A~%" result)
      (assert result nil "Basic dry run failed"))))

(defun test-dry-run-until-step ()
  "Test dry run with specific target step"
  (format t "~%Test: Dry Run Until Step~%")
  (format t "~70,,,'-A~%" "")

  ;; Test each step
  (dolist (step '(:prepare :get :chroot :conf :build))
    (format t "Testing dry-run-until=~A~%" step)
    
    ;; Create build context with dry run enabled and target step
    (let* ((working-dir (uiop:getcwd))
           (output-dir (ensure-directory 
                        (path-join working-dir "output")))
           (context (make-build-context
                     :config *config*
                     :working-dir working-dir
                     :output-dir output-dir
                     :verbose t
                     :dry-run t
                     :dry-run-until step)))
      
      ;; Run the build with dry run enabled and target step
      (let ((result (run-build context)))
        (format t "Build result: ~A~%" result)
        (assert result nil "Dry run until ~A failed" step)))))

(defun test-step-skipping ()
  "Test that steps beyond the target are skipped"
  (format t "~%Test: Step Skipping~%")
  (format t "~70,,,'-A~%" "")

  ;; Create build context with dry run enabled and target step
  (let* ((working-dir (uiop:getcwd))
         (output-dir (ensure-directory 
                      (path-join working-dir "output")))
         (context (make-build-context
                   :config *config*
                   :working-dir working-dir
                   :output-dir output-dir
                   :verbose t
                   :dry-run t
                   :dry-run-until :prepare)))
    
    ;; Count executed vs skipped steps
    (let ((executed-steps 0)
          (skipped-steps 0)
          (expected-executed 1)  ; Only prepare should execute
          (expected-skipped 4))  ; The rest should be skipped
      
      ;; Override log-message to count executed and skipped steps
      (flet ((count-log-message (level format-string &rest args)
               (declare (ignore level format-string))
               (when (and (>= (length args) 3)
                          (stringp (first args))
                          (search "Executing build step" (first args)))
                 (incf executed-steps))
               (when (and (>= (length args) 3)
                          (stringp (first args))
                          (search "Skipping build step" (first args)))
                 (incf skipped-steps))))
        
        ;; Run the build with our counting function
        (let ((old-log-fn (symbol-function 'log-message)))
          (setf (symbol-function 'log-message) #'count-log-message)
          (run-build context)
          (setf (symbol-function 'log-message) old-log-fn)))
      
      ;; Verify results
      (format t "Executed steps: ~A (expected ~A)~%" executed-steps expected-executed)
      (format t "Skipped steps: ~A (expected ~A)~%" skipped-steps expected-skipped)
      (assert (= executed-steps expected-executed) nil 
              "Wrong number of executed steps: ~A (expected ~A)" 
              executed-steps expected-executed)
      (assert (= skipped-steps expected-skipped) nil 
              "Wrong number of skipped steps: ~A (expected ~A)" 
              skipped-steps expected-skipped))))

(defun run-tests ()
  "Run all dry run tests"
  (format t "~%OneFileLinux Dry Run Tests~%")
  (format t "========================~%")
  
  (handler-case
      (progn
        (test-dry-run-basic)
        (test-dry-run-until-step)
        (test-step-skipping)
        
        (format t "~%All tests passed!~%"))
    
    (error (e)
      (format t "~%TEST FAILED: ~A~%" e)
      (uiop:quit 1)))
  
  (uiop:quit 0))

;; Execute tests when script is loaded directly
(eval-when (:execute)
  (run-tests))