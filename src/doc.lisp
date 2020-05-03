(in-package :micmac)

;;;; Register in PAX World

(defun pax-sections ()
  (list @micmac-manual))
(defun pax-pages ()
  `((:objects
     (, @micmac-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :micmac
                      "https://github.com/melisgl/micmac"))))
(register-doc-in-pax-world :micmac (pax-sections) (pax-pages))

#+nil
(progn
  (update-asdf-system-readmes @micmac-manual :micmac)
  (update-asdf-system-html-docs @micmac-manual :micmac
                                :pages (pax-pages)))
