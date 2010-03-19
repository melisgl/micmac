(in-package :micmac.uct)

;;;; Random walk starting from 0. Positive player (moves at even
;;;; depths) wins if game ends in positive land, negative player if it
;;;; ends in the negative else it's a draw.

(defclass random-walk-node (uct-node) ())
(defclass random-walk-edge (uct-edge) ())

;;; Either move to the left or the right.
(defmethod list-edges ((node random-walk-node) state)
  (list (make-instance 'random-walk-edge :action -1 :from-node node)
        (make-instance 'random-walk-edge :action 1 :from-node node)))

;;; The current state is an integer, the action can be simply added to
;;; it.
(defmethod state ((node random-walk-node) parent edge parent-state)
  (+ parent-state (action edge)))

;;; Positive player to maximize, negative to minimize.
(defmethod outcome->reward ((node random-walk-node) outcome)
  (if (evenp (depth node))
      outcome
      (- outcome)))

;;; This is used by EDGE-SCORE. Since the sign of reward of nodes
;;; depend on the parity of depth it must be negated to because
;;; SELECT-EDGE goes for the maximum reward.
(defmethod average-reward ((edge random-walk-edge))
  (- (call-next-method)))

(defun sign (x)
  (cond ((plusp x) 1)
        ((minusp x) -1)
        (t 0)))

;;; The outcome is from the point of view of the first player to move,
;;; i.e. the positive one.
(defmethod play-out ((node random-walk-node) state path)
  (sign (+ state (loop repeat 1
                       sum (if (zerop (random 2))
                               -1
                               1)))))


;;;; Same thing but with the reward being from the point of view of
;;;; the starting player at all depth. Instead of having to play
;;;; negation tricks with OUTCOME->REWARD and AVERAGE-REWARD, here we
;;;; specialize only AVERAGE-REWARD.

(defclass random-walk-node2 (uct-node) ())
(defclass random-walk-edge2 (uct-edge) ())

(defmethod average-reward ((edge random-walk-edge2))
  (if (evenp (depth (from-node edge)))
      (call-next-method)
      (- (call-next-method))))

(defmethod state ((node random-walk-node2) parent edge parent-state)
  (+ parent-state (action edge)))

(defmethod list-edges ((node random-walk-node2) state)
  (list (make-instance 'random-walk-edge2 :action -1 :from-node node)
        (make-instance 'random-walk-edge2 :action 1 :from-node node)))

(defmethod play-out ((node random-walk-node2) state path)
  (sign (+ state (loop repeat 1
                       sum (if (zerop (random 2))
                               -1
                               1)))))


;;;; Test

(defun test-uct-random-walk (root-node-class)
  (let* ((root (uct :root (make-instance root-node-class)
                    :fresh-root-state (lambda () 0)
                    :exploration-bias 1
                    :max-n-playouts 100))
         (visited-edges (visited-edges root)))
    (assert (endp (unvisited-edges root)))
    (assert (and visited-edges
                 (eq 1 (action (extremum visited-edges #'> :key #'n-visits)))))
    (let* ((edge (find 1 (edges root) :key #'action))
           (node (to-node edge))
           (visited-edges (visited-edges node)))
      (assert (endp (unvisited-edges node)))
      (assert (and visited-edges
                   (eq -1 (action (extremum visited-edges #'>
                                            :key #'n-visits))))))))

(defun test-uct ()
  (loop repeat 100 do (test-uct-random-walk 'random-walk-node))
  (loop repeat 100 do (test-uct-random-walk 'random-walk-node2)))

;;(test-uct)
