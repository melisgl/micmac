;;;; -*- mode: Lisp -*-

(asdf:defsystem #:micmac
  :licence "MIT, see COPYING."
  :version "0.0.2"
  :author "Gábor Melis <mega@retes.hu>"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/mgl-gpr"
  :bug-tracker "https://github.com/melisgl/mgl-gpr/issues"
  :source-control (:git "https://github.com/melisgl/mgl-gpr.git")
  :name "Micmac, graph search library and bucket of random bits"
  :description "Micmac is mainly a library of graph search algorithms
  such as alpha-beta, UCT and beam search, but it also has some MCMC
  and other slightly unrelated stuff."
  :depends-on (#:alexandria #:mgl-pax)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "uct")
                                     (:file "graph-search")
                                     (:file "metropolis-hastings")
                                     (:file "game-theory")
                                     (:file "micmac")
                                     (:file "doc"))))
  :in-order-to ((asdf:test-op (asdf:test-op "micmac/test"))))

(asdf:defsystem micmac/test
  :licence "MIT, see COPYING."
  :author "Gábor Melis <mega@retes.hu>"
  :mailto "mega@retes.hu"
  :description "Test system for Micmac."
  :depends-on (#:micmac)
  :components ((:module "test"
                :serial t
                :components ((:file "test-alpha-beta")
                             (:file "test-beam-search")
                             (:file "test-uct")
                             (:file "test-metropolis-hastings")
                             (:file "test-game-theory")
                             (:file "test"))))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:micmac '#:test)))
