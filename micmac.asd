;;;; -*- mode: Lisp -*-

(asdf:defsystem #:micmac
  :name "Micmac"
  :author "Gabor Melis"
  :version "0.0.1"
  :licence "MIT"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "metropolis-hastings")
                             (:file "uct")
                             (:file "game-theory"))))
  :serial t)

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:micmac))))
  (asdf:oos 'asdf:load-op '#:micmac-test)
  (funcall (intern (symbol-name '#:test) (find-package '#:micmac))))

(defmethod asdf:operation-done-p ((o asdf:test-op)
                                  (c (eql (asdf:find-system '#:micmac))))
  (values nil))
