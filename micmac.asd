;;;; -*- mode: Lisp -*-

(asdf:defsystem #:micmac
  :licence "MIT, see COPYING."
  :version "0.0.2"
  :name "Micmac, graph search library and bucket of random bits"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
  :description "Micmac is mainly a library of graph search algorithms
  such as alpha-beta, UCT and beam search, but it also has some MCMC
  and other slightly unrelated stuff."
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "uct")
                             (:file "graph-search")
                             (:file "metropolis-hastings")
                             (:file "game-theory")
                             (:file "micmac")
                             (:file "doc"))))
  :serial t
  :depends-on (#:mgl-pax))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:micmac))))
  (asdf:oos 'asdf:load-op '#:micmac-test)
  (funcall (intern (symbol-name '#:test) (find-package '#:micmac))))
