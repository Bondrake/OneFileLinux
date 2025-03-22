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
   (:module "steps"
    :components
    ((:file "prepare" :depends-on ("core" "config" "build"))
     (:file "get" :depends-on ("core" "config" "build"))
     (:file "chrootandinstall" :depends-on ("core" "config" "build"))
     (:file "conf" :depends-on ("core" "config" "build"))
     (:file "build" :depends-on ("core" "config" "build" "kernel/config-utils" "package/apk-builder"))))
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

;; Define package aliases for backwards compatibility
(defpackage :onefilelinux.util
  (:use :cl)
  (:documentation "Compatibility package for older code"))