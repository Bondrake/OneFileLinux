# OneFileLinux Refactored Implementation Plan

This document outlines a practical plan for completing the implementation of the refactored OneFileLinux build system to ensure it can run successfully.

## Phase 1: Core System Completion

### Task 1.1: ASDF System Definition

Create an ASDF system definition that properly specifies dependencies and component relationships.

**File**: `onefilelinux.asd`

**Content**:
```lisp
(asdf:defsystem "onefilelinux"
  :description "OneFileLinux Build System"
  :version "1.0.0"
  :author "OneFileLinux Team"
  :license "MIT"
  :depends-on (:uiop :cl-ppcre :alexandria)
  :components
  ((:file "core")
   (:file "config" :depends-on ("core"))
   (:file "build" :depends-on ("core" "config"))
   (:module "steps"
    :components
    ((:file "prepare" :depends-on ("core" "config" "build"))
     (:file "get" :depends-on ("core" "config" "build"))
     (:file "chrootandinstall" :depends-on ("core" "config" "build"))
     (:file "conf" :depends-on ("core" "config" "build"))
     (:file "build" :depends-on ("core" "config" "build" "kernel" "package"))))
   (:module "kernel"
    :components
    ((:file "config-utils" :depends-on ("core" "config"))))
   (:module "package"
    :components
    ((:file "apk-builder" :depends-on ("core" "config"))))
   (:module "docker"
    :components
    ((:file "build-onefilelinux" :depends-on ("core" "config" "build"))
     (:file "auto-resources" :depends-on ("core"))))
   (:module "github"
    :components
    ((:file "actions-helper" :depends-on ("core" "config"))))))
```

### Task 1.2: Main Entry Point

Create a main entry point script that loads the system and processes command-line arguments.

**File**: `main.lisp`

**Content**:
```lisp
;;;; OneFileLinux Main Entry Point
(require :asdf)
(asdf:load-system "onefilelinux")

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (handler-case
        (onefilelinux.build:main args)
      (error (e)
        (format *error-output* "ERROR: ~A~%" e)
        (uiop:quit 1)))))

(main)
```

### Task 1.3: Complete Missing Utilities

Add missing utility functions referenced in the codebase.

**File**: `core.lisp` (additions)

**Content**:
```lisp
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
```

## Phase 2: Build System Fixes

### Task 2.1: Permission and Privilege Handling

Add proper handling for permissions and privileges.

**File**: `steps/prepare.lisp` (additions)

**Content**:
```lisp
(defun check-privileges ()
  "Check if running with sufficient privileges"
  (log-message :debug "Checking privileges")
  (let ((is-root (zerop (parse-integer 
                         (shell-command-to-string "id -u")))))
    (unless is-root
      (log-message :warning "Not running as root. Some operations may fail.")
      (when (config-value 'build-config 'require-root t)
        (error 'onefilelinux-error
               :message "Root privileges required for build operations. Please run as root or with sudo.")))
    is-root))

(defun ensure-writable-directory (path)
  "Ensure directory exists and is writable"
  (log-message :debug "Ensuring directory is writable: ~A" path)
  (ensure-directory path)
  (unless (run-command-status (format nil "test -w ~A" path))
    (error 'file-system-error
           :message "Directory not writable"
           :path path)))
```

### Task 2.2: Configuration Loading

Add proper configuration loading functionality.

**File**: `config.lisp` (additions)

**Content**:
```lisp
(defun load-config-file (path)
  "Load configuration from a file"
  (log-message :info "Loading configuration from ~A" path)
  (if (file-exists-p path)
      (let ((config (with-open-file (in path :direction :input)
                      (read in))))
        (validate-config config)
        (setf *config* config)
        config)
      (error 'file-system-error
             :message "Configuration file not found"
             :path path)))

(defun validate-config (config)
  "Validate configuration structure"
  (log-message :debug "Validating configuration")
  ;; Basic validation - ensure it's a hash table
  (unless (hash-table-p config)
    (error 'configuration-error
           :message "Configuration must be a hash table"))
  
  ;; Check required sections
  (dolist (section '(:system :build :features :packages))
    (unless (gethash section config)
      (error 'configuration-error
             :message (format nil "Missing required configuration section: ~A" section)
             :key section)))
  
  config)
```

## Phase 3: Docker Integration Completion

### Task 3.1: Docker Entrypoint Script

Create an entrypoint script for the Docker container.

**File**: `docker/entrypoint.sh`

**Content**:
```bash
#!/bin/bash
# Docker entrypoint for OneFileLinux build

# Setup environment
export ONEFILELINUX_ROOT="/build"
export ONEFILELINUX_OUTPUT="/output"
export ONEFILELINUX_LOG_FILE="/var/log/onefilelinux-build.log"

# Parse arguments
ARGS=""
for arg in "$@"; do
    ARGS="$ARGS \"$arg\""
done

# Run the build
sbcl --no-sysinit --no-userinit \
     --load /build/onefilelinux.asd \
     --eval "(asdf:load-system :onefilelinux)" \
     --load /build/docker/entrypoint.lisp \
     --eval "(onefilelinux.docker.build:main (list $ARGS))" \
     --quit

# Get build status
BUILD_STATUS=$?

# Copy results to output directory
if [ -d "$ONEFILELINUX_ROOT/output" ]; then
  mkdir -p "$ONEFILELINUX_OUTPUT"
  cp -r "$ONEFILELINUX_ROOT/output"/* "$ONEFILELINUX_OUTPUT/"
  echo "Build artifacts copied to output directory."
fi

# Return build status
exit $BUILD_STATUS
```

### Task 3.2: Docker Resource Detection

Enhance Docker resource detection for containerized environments.

**File**: `docker/auto-resources.lisp` (additions)

**Content**:
```lisp
(defun detect-container-cpu-limit ()
  "Detect CPU limit in a container environment"
  (log-message :debug "Detecting container CPU limits")
  (if (file-exists-p "/sys/fs/cgroup/cpu/cpu.cfs_quota_us")
      (let ((quota (parse-integer 
                    (string-trim '(#\Space #\Tab #\Newline)
                                (read-file-content "/sys/fs/cgroup/cpu/cpu.cfs_quota_us"))))
            (period (parse-integer 
                     (string-trim '(#\Space #\Tab #\Newline)
                                 (read-file-content "/sys/fs/cgroup/cpu/cpu.cfs_period_us")))))
        (if (> quota 0)
            (ceiling (/ quota period))
            (detect-cpus)))
      (detect-cpus)))

(defun detect-container-memory-limit ()
  "Detect memory limit in a container environment"
  (log-message :debug "Detecting container memory limits")
  (if (file-exists-p "/sys/fs/cgroup/memory/memory.limit_in_bytes")
      (let* ((limit-bytes (parse-integer 
                           (string-trim '(#\Space #\Tab #\Newline)
                                       (read-file-content "/sys/fs/cgroup/memory/memory.limit_in_bytes"))))
             (memory-mb (floor limit-bytes 1048576)))
        (if (< memory-mb (detect-memory-mb))
            memory-mb
            (detect-memory-mb)))
      (detect-memory-mb)))
```

## Phase 4: Testing Framework

### Task 4.1: Basic Test Framework

Create a basic testing framework for the build system.

**File**: `tests/run-tests.lisp`

**Content**:
```lisp
;;;; OneFileLinux Test Framework
(require :asdf)
(asdf:load-system "onefilelinux")

(defpackage :onefilelinux.test
  (:use :cl :onefilelinux.core))

(in-package :onefilelinux.test)

(defun run-tests ()
  (format t "~%Running OneFileLinux tests...~%~%")
  (let ((tests '(test-core-utilities
                test-configuration
                test-build-steps))
        (passed 0)
        (failed 0))
    
    (dolist (test tests)
      (format t "Running test: ~A~%" test)
      (handler-case
          (progn
            (funcall test)
            (format t "  PASSED~%")
            (incf passed))
        (error (e)
          (format t "  FAILED: ~A~%" e)
          (incf failed))))
    
    (format t "~%Test summary: ~D passed, ~D failed~%" passed failed)
    (when (> failed 0)
      (uiop:quit 1))))

(defun test-core-utilities ()
  (assert (string= (path-join "/a" "b") "/a/b"))
  (assert (directory-exists-p "/tmp"))
  (assert (string-prefix-p "abc" "abcdef"))
  (assert (not (string-prefix-p "def" "abcdef")))
  (assert (string-suffix-p "def" "abcdef"))
  (assert (not (string-suffix-p "abc" "abcdef")))
  (assert (equal (split-string "a,b,c" ",") '("a" "b" "c")))
  (assert (string= (string-join '("a" "b" "c") ",") "a,b,c")))

(defun test-configuration ()
  ;; Test config functions when implemented
  t)

(defun test-build-steps ()
  ;; Test build steps when implemented
  t)

(run-tests)
```

### Task 4.2: GitHub Actions Workflow

Create a GitHub Actions workflow for CI/CD.

**File**: `.github/workflows/build.yml`

**Content**:
```yaml
name: Build and Test

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Install SBCL
      run: |
        sudo apt-get update
        sudo apt-get install -y sbcl cl-quicklisp
    
    - name: Run tests
      run: sbcl --load tests/run-tests.lisp --quit
  
  build:
    needs: test
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Build Docker image
      run: docker build -t onefilelinux-builder ./docker
    
    - name: Build minimal profile
      run: |
        mkdir -p output
        docker run -v $(pwd)/output:/output onefilelinux-builder --profile minimal --github-actions
    
    - name: Upload artifacts
      uses: actions/upload-artifact@v2
      with:
        name: onefilelinux-efi
        path: output/*.efi
```

## Phase 5: Documentation

### Task 5.1: User Guide

Create a comprehensive user guide.

**File**: `docs/user-guide.md`

**Content**: 
[Detailed user guide with installation, usage, and troubleshooting information]

### Task 5.2: Developer Guide

Create a developer guide for contributors.

**File**: `docs/developer-guide.md`

**Content**:
[Detailed developer guide with architecture, coding standards, and contribution guidelines]

## Implementation Timeline

1. **Phase 1: Core System Completion**
   - Task 1.1: ASDF System Definition (1 day)
   - Task 1.2: Main Entry Point (1 day)
   - Task 1.3: Complete Missing Utilities (2 days)

2. **Phase 2: Build System Fixes**
   - Task 2.1: Permission and Privilege Handling (1 day)
   - Task 2.2: Configuration Loading (2 days)

3. **Phase 3: Docker Integration Completion**
   - Task 3.1: Docker Entrypoint Script (1 day)
   - Task 3.2: Docker Resource Detection (1 day)

4. **Phase 4: Testing Framework**
   - Task 4.1: Basic Test Framework (2 days)
   - Task 4.2: GitHub Actions Workflow (1 day)

5. **Phase 5: Documentation**
   - Task 5.1: User Guide (2 days)
   - Task 5.2: Developer Guide (2 days)

Total estimated time: 16 days

## Conclusion

This implementation plan addresses all the identified blockers for the refactored OneFileLinux build system. By completing these tasks in the specified order, we will establish a functional build system that can run both directly on Linux and in Docker environments, with proper testing and documentation.