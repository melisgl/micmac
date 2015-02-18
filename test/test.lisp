(in-package :micmac)

(defun test ()
  (test-alpha-beta)
  (test-beam-search)
  (test-parallel-beam-search)
  (micmac.uct::test-uct)
  (micmac.metropolis-hastings::test-metropolis-hastings)
  (micmac.game-theory::test-game-theory))
