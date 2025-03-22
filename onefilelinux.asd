;;;; OneFileLinux Build System - ASDF System Definition
;;;; This file defines the system and its dependencies for ASDF

(asdf:defsystem "onefilelinux"
  :description "OneFileLinux Build System - Refactored Implementation"
  :version "1.0.0"
  :author "OneFileLinux Team"
  :license "MIT"
  :depends-on (:uiop :cl-ppcre :alexandria)
  :serial nil
  :components
  ((:file "core")
   (:file "config" :depends-on ("core"))
   (:file "build" :depends-on ("core" "config"))
   (:module "kernel"
    :depends-on ("core" "config")
    :components
    ((:file "config-utils")))
   (:module "package"
    :depends-on ("core" "config")
    :components
    ((:file "apk-builder")))
   (:module "steps"
    :depends-on ("core" "config" "build" "kernel" "package")
    :components
    ((:file "prepare")
     (:file "get")
     (:file "chrootandinstall")
     (:file "conf")
     (:file "build")))
   (:module "docker"
    :depends-on ("core" "config" "build")
    :components
    ((:file "build-onefilelinux")
     (:file "auto-resources" :depends-on ("core"))))
   (:module "github"
    :depends-on ("core" "config")
    :components
    ((:file "actions-helper")))))

;; Define package aliases for backwards compatibility
(defpackage :onefilelinux.util
  (:use :cl)
  (:documentation "Compatibility package for older code"))