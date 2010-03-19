;;;; -*- mode: Lisp -*-

(asdf:defsystem #:micmac-test
  :name "Tests for Micmac"
  :author "Gabor Melis"
  :version "0.0.0"
  :licence "MIT"
  :components ((:module "test"
                :serial t
                :components ((:file "test-metropolis-hastings")
                             (:file "test-uct")
                             (:file "test"))))
  :depends-on (#:micmac))
