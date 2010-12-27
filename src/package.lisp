(cl:defpackage :micmac
  (:use :common-lisp)
  (:export))

(cl:defpackage :micmac.metropolis-hastings
  (:nicknames #:micmac.mh)
  (:use :common-lisp)
  (:export
   #:mc-chain
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

(cl:defpackage :micmac.uct
  (:use :common-lisp)
  (:export 
   ;; Node
   #:uct-node
   #:depth
   #:n-visits
   #:edges
   #:average-reward
   ;; Edge
   #:uct-edge
   #:action
   #:from-node
   #:to-node
   ;; Misc
   #:visited-edges
   #:unvisited-edges
   #:edge-score
   #:select-edge
   ;; Oft-specialized generic functions
   #:outcome->reward
   #:update-uct-statistics
   #:make-uct-node
   #:state
   #:list-actions
   #:play-out
   ;; Beef
   #:uct)
  (:documentation "UCT Monte Carlo tree search."))

(cl:defpackage :micmac.game-theory
  (:use :common-lisp)
  (:export
   #:find-nash-equilibrium
   #:alpha-beta)
  (:documentation "Game theory."))
