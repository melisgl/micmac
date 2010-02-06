(cl:defpackage :micmac
  (:use :common-lisp)
  (:export))

(cl:defpackage :micmac.metropolis-hastings
  (:nicknames #:micmac.mh)
  (:use :common-lisp)
  (:export #:mc-chain
           #:temperature
           #:state
           #:jump-to-sample
           #:jump-to-sample*
           #:prepare-jump-distribution
           #:random-jump
           #:log-probability-ratio
           #:log-probability-ratio-to-jump-target
           #:log-jump-probability-ratio
           #:acceptance-probability
           #:accept-jump
           #:reject-jump
           #:mayb-jump
           #:jump
           ;; MC3
           #:mc3-chain
           #:accept-swap-chain-states
           #:reject-swap-chain-states
           #:maybe-swap-chain-states
           #:jump-between-chains
           ;; Utility classes
           #:enumerating-chain
           #:tracing-chain)
  (:documentation "Metroplis-Hastings MCMC."))
