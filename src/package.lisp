(mgl-pax:define-package :micmac
  (:documentation "See MICMAC:@MICMAC-MANUAL.")
  (:use #:common-lisp #:mgl-pax))

(mgl-pax:define-package :micmac.metropolis-hastings
  (:nicknames #:micmac.mh)
  (:use :common-lisp :mgl-pax)
  (:documentation "See
  MICMAC.METROPOLIS-HASTINGS:@MICMAC-METROPOLIS-HASTINGS."))

(mgl-pax:define-package :micmac.uct
  (:use :common-lisp :mgl-pax)
  (:documentation "See MICMAC.UCT:@MICMAC-UCT."))

(mgl-pax:define-package :micmac.game-theory
  (:use :common-lisp :mgl-pax)
  (:documentation "See MICMAC.GAME-THEORY:@MICMAC-GAME-THEORY."))
