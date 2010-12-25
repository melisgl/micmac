;;;; Generic interface for the Metropolis-Hastings algorithm, also
;;;; Metropolis Coupled MCMC.
;;;;
;;;; References:
;;;;
;;;; - http://en.wikipedia.org/wiki/Metropolis–Hastings_algorithm
;;;;
;;;; - Markov Chain Monte Carlo and Gibbs Sampling
;;;;   Lecture Notes for EEB 581, version 26 April 2004 c B. Walsh 2004
;;;;   http://web.mit.edu/~wingated/www/introductions/mcmc-gibbs-intro.pdf
;;;;
;;;; - Geyer, C.J. (1991) Markov chain Monte Carlo maximum likelihood

(in-package :micmac.metropolis-hastings)

(defclass mc-chain ()
  ((temperature
    :initform 1d0 :initarg :temperature :accessor temperature
    :documentation "The PROBABILITY-RATIO of samples is raised to the
power of 1 / TEMPERATURE before calculating the acceptance
probability. This effectively flattens the peaks if TEMPERATURE > 1
which makes it easier for the chain to traverse deep valleys.")
   (state
    :initarg :state :reader state
    :documentation "This is the current sample where the chain is.")
   (candidate
    :initform nil :accessor candidate)
   (jump-distribution-prepared-p
    :initform nil :accessor jump-distribution-prepared-p))
  (:documentation "A simple markov chain for Metropolis Hastings. With
temperature it is suitable for MC3."))

(defgeneric set-state (state chain)
  (:method (state chain)
    (setf (jump-distribution-prepared-p chain) nil)
    (setf (slot-value chain 'state) state)))

(defsetf state (object) (store)
  `(set-state ,store ,object))

(defun jump-to-sample (chain jump &key (result-sample (state chain)))
  "From the current state of CHAIN make JUMP (from the current
distribution of CHAIN) and return the sample where we landed. Reuse
RESULT-SAMPLE when possible. "
  (jump-to-sample* chain jump result-sample))

(defgeneric jump-to-sample* (chain jump result-sample)
  (:documentation "This function is called by JUMP-TO-SAMPLE. It is
where JUMP-TO-SAMPLE behaviour shall be customized."))

(defgeneric prepare-jump-distribution (chain)
  (:documentation "Prepare for sampling from the F(X) = Q(SAMPLE->X)
distribution. Called by RANDOM-JUMP. The around method ensures that
nothing is done unless there was a state change.")
  (:method :around (chain)
   (unless (jump-distribution-prepared-p chain)
     (call-next-method)
     (setf (jump-distribution-prepared-p chain) t))))

(defgeneric random-jump (chain)
  (:documentation "Sample a jump from the current distribution of
jumps that was computed by PREPARE-JUMP-DISTRIBUTION.")
  (:method :before (chain)
   (prepare-jump-distribution chain)))

(defgeneric log-probability-ratio (chain sample1 sample2)
  (:documentation "Return P(SAMPLE1)/P(SAMPLE2). It's in the log
domain to avoid overflows and the ratio part is because that it may
allow computational shortcuts as opposed to calculating unnormalized
probabilities separately."))

(defgeneric log-probability-ratio-to-jump-target (chain jump target)
  (:documentation "Return P(TARGET)/P(STATE) where JUMP is from the
current state of CHAIN to TARGET sample. This can be specialized for
speed. The default implementation just falls back on
LOG-PROBABILITY-RATIO.")
  (:method (chain jump target)
    (log-probability-ratio chain target (state chain))))

(defgeneric log-jump-probability-ratio (chain jump target)
  (:documentation "Return Q(TARGET->STATE) / Q(STATE->TARGET) where Q
is the jump distribution and JUMP is from the current STATE of CHAIN
to TARGET sample."))

;;; Like EXP, but avoid overflows.
(defun log->acceptance-probability (x)
  (if (plusp x)
      1d0
      (exp x)))

(defgeneric acceptance-probability (chain jump candidate)
  (:documentation "Calculate the acceptance probability of CANDIDATE
to which JUMP leads from the current STATE of CHAIN.")
  (:method (chain jump candidate)
    (log->acceptance-probability
     (+ (/ (log-probability-ratio-to-jump-target chain jump candidate)
           (temperature chain))
        (log-jump-probability-ratio chain jump candidate)))))

(defgeneric accept-jump (chain jump candidate)
  (:documentation "Called when CHAIN accepts JUMP to CANDIDATE.")
  (:method (chain jump candidate)
    ;; When the state is mutable it is important the we swap STATE and
    ;; CANDIDATE.
    (rotatef (state chain) (candidate chain))
    ;; This is a NOP as of now, but let's prepare of the case where
    ;; CANDIDATE is not the same as the cached CANDIDATE slot.
    (setf (state chain) candidate)))

(defgeneric reject-jump (chain jump candidate)
  (:documentation "Called when CHAIN rejects JUMP to CANDIDATE. It
does nothing by default, it's just a convenience for debugging.")
  (:method (chain jump candidate)))

(defgeneric maybe-jump (chain jump candidate acceptance-probability)
  (:documentation "Randomly accept or reject JUMP to CANDIDATE from
the current state of CHAIN with ACCEPTANCE-PROBABILITY.")
  (:method (chain jump candidate acceptance-probability)
    (cond ((< (random 1d0) acceptance-probability)
           (accept-jump chain jump candidate)
           t)
          (t
           (reject-jump chain jump candidate)
           nil))))

(defgeneric jump (chain)
  (:documentation "Take a step on the markov chain. Return a boolean
indicating whether the proposed jump was accepted.")
  (:method (chain)
    (with-accessors ((candidate candidate)) chain
      (let ((jump (random-jump chain)))
        (setf candidate
              (jump-to-sample chain jump :result-sample candidate))
        (let ((acceptance-probability
               (acceptance-probability chain jump candidate)))
          (maybe-jump chain jump candidate acceptance-probability))))))


;;;; Metropolis Coupled MCMC (MCMCMC, aka MC^3)

(defclass mc3-chain (mc-chain)
  ((hot-chains
    :type list :initform () :initarg :hot-chains :reader hot-chains))
  (:documentation "High probability island separated by low valley
make the chain poorly mixing. MC3-CHAIN has a number of HOT-CHAINS
that have state probabilities similar to that of the main chain but
less jagged. Often it suffices to set the temperatures of the
HOT-CHAINS higher use the very same base probability distribution."))

(defgeneric accept-swap-chain-states (mc3 chain1 chain2)
  (:documentation "Swap the states of CHAIN1 and CHAIN2 of MC3.")
  (:method (mc3 chain1 chain2)
    (rotatef (state chain1) (state chain2))))

(defgeneric reject-swap-chain-states (mc3 chain1 chain2)
  (:documentation "Called when the swap of states of CHAIN1 and CHAIN2
is rejected. It does nothing by default, it's just a convenience for
debugging.")
  (:method (mc3 chain1 chain2)))

(defun random-until-different (limit tabu &key (test #'eql))
  (loop for x = (random limit)
        while (funcall test x tabu)
        finally (return x)))

(defgeneric maybe-swap-chain-states (mc3 chain1 chain2 acceptance-probability)
  (:documentation "Swap of states of CHAIN1 and CHAIN2 of MC3 with
ACCEPTANCE-PROBABILITY.")
  (:method (mc3 chain1 chain2 acceptance-probability)
    (cond ((< (random 1d0) acceptance-probability)
           (accept-swap-chain-states mc3 chain1 chain2)
           t)
          (t
           (reject-swap-chain-states mc3 chain1 chain2)
           nil))))

(defgeneric jump-between-chains (mc3)
  (:documentation "Choose two chains randomly and swap their states
with MC3 acceptance probability.")
  (:method (mc3)
    (let* ((chains (cons mc3 (hot-chains mc3)))
           (n (length chains)))
      (when (< 1 n)
        (let* ((i (random n))
               (j (random-until-different n i))
               (chain-i (elt chains i))
               (chain-j (elt chains j)))
          (and (maybe-swap-chain-states
                mc3 chain-i chain-j
                (log->acceptance-probability
                 (* (log-probability-ratio (elt chains 0)
                                           (state chain-i) (state chain-j))
                    (- (/ (temperature chain-j))
                       (/ (temperature chain-i))))))
               (or (zerop i) (zerop j))))))))

(defmethod jump ((chain mc3-chain))
  (let ((cold-chain-changed-p (call-next-method)))
    (map nil #'jump (hot-chains chain))
    (when (jump-between-chains chain)
      (setq cold-chain-changed-p t))
    cold-chain-changed-p))


;;;; Enumerating chain

(defun sum-seq (seq &key (key #'identity) (start 0) (end (length seq)))
  "Return the sum of elements in the [START,END) subsequence of SEQ."
  (if (typep seq 'list)
      (loop repeat (- end start)
            for l = (nthcdr start seq) then (cdr l)
            sum (funcall key (car l)))
      (loop for i upfrom start below end
            sum (funcall key (aref seq i)))))

(defun random-element (seq &key (key #'identity)
                       (start 0) (end (length seq))
                       (sum (sum-seq seq :key key :start start :end end)))
  "Choose an element randomly from the [START,END) subsequence of SEQ
with given probabilities. KEY returns the unormalized probability of
an element, SUM is the sum of these unnormalized probalities contains
unnormalized probabilties. Return the element chosen and its index."
  (let ((x (random (float sum 0d0))))
    (do* ((i start (1+ i))
          (e (elt seq i) (elt seq i))
          (s (funcall key e) (+ s (funcall key e))))
         ((or (<= x s) (>= i (1- end))) (values e i)))))

(defclass enumerating-chain (mc-chain)
  ((p-jumps :reader p-jumps))
  (:documentation "A simple abstract chain subclass that
explicitly enumerates the probabilities of the distribution."))

(defmethod initialize-instance :after ((chain enumerating-chain)
                                       &key n-jumps &allow-other-keys)
  (unless (slot-boundp chain 'p-jumps)
    (setf (slot-value chain 'p-jumps) (make-array n-jumps))))

(defmethod random-jump ((chain enumerating-chain))
  (nth-value 1 (random-element (p-jumps chain) :sum 1d0)))


;;;; Tracing chain

(defclass tracing-chain () 
  ()
  (:documentation "Mix this in with your chain to have it print
trace of acceptances/rejections."))

(defmethod maybe-jump ((chain tracing-chain) jump candidate
                       acceptance-probability)
  (format *trace-output* "Considering jump ~A with probability ~A ... "
          jump acceptance-probability)
  (call-next-method))

(defmethod accept-jump ((chain tracing-chain) jump candidate)
  (format *trace-output* "accepted~%")
  (call-next-method))

(defmethod reject-jump ((chain tracing-chain) jump candidate)
  (format *trace-output* "rejected~%")
  (call-next-method))

(defmethod maybe-swap-chain-states ((chain tracing-chain) chain1 chain2
                                    acceptance-probability)
  (format *trace-output*
          "Considering swapping states of chains with temperatures ~A and ~A ~%~
           with probability ~A ... "
          (temperature chain1) (temperature chain2) acceptance-probability)
  (call-next-method))

(defmethod accept-swap-chain-states ((chain tracing-chain) chain1 chain2)
  (format *trace-output* "accepted~%")
  (call-next-method))

(defmethod reject-swap-chain-states ((chain tracing-chain) chain1 chain2)
  (format *trace-output* "rejected~%")
  (call-next-method))
