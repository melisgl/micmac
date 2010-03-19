(in-package :micmac.metropolis-hastings)

(defun test-sum-seq ()
  (assert (= 6 (sum-seq '(0 1 2 3 4 5) :start 1 :end 4)))
  (assert (= 6 (sum-seq #(0 1 2 3 4 5) :start 1 :end 4))))

(defun test-random-element ()
  (assert (equal '(1 0) (multiple-value-list (random-element #(1 0))))))

(defun test-util ()
  (test-random-element)
  (test-sum-seq))


;;;; Utilities

(defun gaussian-pdf (x mean variance)
  (/ (exp (- (/ (expt (- x mean) 2)
                (* 2 variance))))
     (sqrt (* 2 pi variance))))

(defun gaussian-random-1 ()
  "Return a single float of zero mean and unit variance."
  (loop
   (let* ((x1 (1- (* 2d0 (random 1d0))))
          (x2 (1- (* 2d0 (random 1d0))))
          (w (+ (* x1 x1) (* x2 x2))))
     (declare (type double-float x1 x2)
              (type (double-float 0d0) w))
     (when (< w 1.0)
       ;; Now we have two random numbers but return only one. The
       ;; other would be X1 times the same.
       (return (* x2 (sqrt (/ (* -2.0 (log w)) w))))))))


;;;; Sampling from a normal distribution with normally distributed
;;;; jumps.
;;;;
;;;; A jump is simply the delta from the current state.

;;; One could mix TRACING-CHAIN in to debug.
(defclass normal-chain (mc-chain)
  ((chain-mean :initarg :chain-mean :reader chain-mean)
   (chain-variance :initarg :chain-variance :reader chain-variance)
   (jump-variance :initarg :jump-variance :reader jump-variance)))

;;; Teach it how to make a jump that was sampled.
(defmethod jump-to-sample* ((chain normal-chain) jump result-sample)
  (+ (state chain) jump))

;;; RANDOM-JUMP is called from the same state until a jump is finally
;;; accepted. Often there are calculations that can be done
;;; beforehand, instead of in each RANDOM-JUMP, but here we don't need
;;; to to any of that.
(defmethod prepare-jump-distribution ((chain normal-chain)))

(defmethod random-jump ((chain normal-chain))
  (* (sqrt (jump-variance chain))
     (gaussian-random-1)))

(defmethod log-probability-ratio ((chain normal-chain) x y)
  (log (/ (gaussian-pdf x (chain-mean chain) (chain-variance chain))
          (gaussian-pdf y (chain-mean chain) (chain-variance chain)))))

(defmethod log-jump-probability-ratio ((chain normal-chain) x y)
  ;; It's symmetric: Q(X->Y) = Q(Y->X), thus Q(X->Y)/Q(Y->X) is 1.
  0d0)

(defclass normal-mc3-chain (mc3-chain normal-chain) ())


;;;; Test

(defun run-chain (chain n-samples)
  (let ((samples (make-array n-samples)))
    (loop for i below n-samples do
          (jump chain)
          (setf (aref samples i) (state chain)))
    samples))

(defun mean (vector)
  (/ (loop for x across vector
           sum x)
     (length vector)))

(defun variance (vector)
  (let ((mean (mean vector)))
    (/ (loop for x across vector
             sum (expt (- x mean) 2))
       (length vector))))

(defun test-normal-chain (&key chain-mean chain-variance jump-variance
                          (n-samples 100000) n-hot-chains)
  (flet ((make-normal-chain (class &rest initargs)
           (apply #'make-instance class
                  :state 1d0
                  :chain-mean chain-mean
                  :chain-variance chain-variance
                  :jump-variance jump-variance
                  initargs)))
    (let* ((chain
            (if n-hot-chains
                (make-normal-chain
                 'normal-mc3-chain
                 :hot-chains (loop repeat n-hot-chains
                                   for i upfrom 1
                                   collect (make-normal-chain
                                            'normal-chain
                                            :temperature
                                            (1+ (* i 8d0)))))
                (make-normal-chain 'normal-chain)))
           (samples (run-chain chain n-samples)))
      (assert (> 0.1 (abs (- chain-mean (mean samples)))))
      (assert (> 0.1 (abs (- chain-variance (variance samples))))))))

(defun test-plain-metropolis-hastings ()
  (test-normal-chain :chain-mean 1d0 :chain-variance 2d0 :jump-variance 1d0)
  (test-normal-chain :chain-mean 10d0 :chain-variance 0.5d0 :jump-variance 4d0)
  (test-normal-chain :chain-mean 0.2d0 :chain-variance 4d0 :jump-variance 8d0))

(defun test-mc3 ()
  (test-normal-chain :chain-mean 1d0 :chain-variance 2d0 :jump-variance 1d0
                     :n-hot-chains 1)
  (test-normal-chain :chain-mean 10d0 :chain-variance 0.5d0 :jump-variance 4d0
                     :n-hot-chains 2)
  (test-normal-chain :chain-mean 0.2d0 :chain-variance 4d0 :jump-variance 8d0
                     :n-hot-chains 4))

(defun test-metropolis-hastings ()
  (test-util)
  (test-plain-metropolis-hastings)
  (test-mc3))
