;;;; -*- mode: Lisp -*-

(asdf:defsystem #:micmac-test
  :name "Tests for Micmac"
  :version "0.0.0"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
  :licence "MIT"
  :description "Test system for Micmac."
  :components ((:module "test"
                :serial t
                :components ((:file "test-alpha-beta")
                             (:file "test-beam-search")
                             (:file "test-uct")
                             (:file "test-metropolis-hastings")
                             (:file "test-game-theory")
                             (:file "test"))))
  :depends-on (#:micmac))
