(in-package :micmac)

(defun test ()
  (micmac.metropolis-hastings::test-metropolis-hastings)
  (micmac.uct::test-uct)
  (micmac.game-theory::test-game-theory)
  (test-alpha-beta))
