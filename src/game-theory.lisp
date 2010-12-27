(in-package :micmac.game-theory)

(defun extremum (vector pred &key (start 0) (end (length vector)))
  "Find the first extreme value of the [START,END) subsequence of
VECTOR and return it along with its index. The extremum is what would
be the first element of VECTOR sorted by SORT with PRED."
  (let ((extremum-index nil)
        (extremum nil))
    (loop for i upfrom start below end
          do (let ((x (aref vector i)))
               (when (or (null extremum-index)
                         (funcall pred x extremum))
                 (setq extremum-index i
                       extremum x))))
    (values extremum extremum-index)))

(defun list-to-2d-matrix (list)
  (make-array (list (length list) (length (first list)))
              :initial-contents list))

;;; Approximate the strategy oddments for 2 person zero-sum games of
;;; perfect information.
;;;
;;; Applies the iterative solution method described by J.D. Williams
;;; in his classic book, The Compleat Strategyst, ISBN 0-486-25101-2.
;;; See chapter 5, page 180 for details.
;;;
;;; Translated from http://code.activestate.com/recipes/496825/
(defun find-nash-equilibrium (payoff &key (n-iterations 100))
  "Find a Nash equilibrium and for a zero-sum game represented by
PAYOFF matrix (a 2d matrix or a nested list). PAYOFF is from the point
of view of the row player: the player who choses column wants to
minimize, the row player wants to maximize. The first value returned
is a vector of unnormalized probabilities assigned to each action of
the row player, the second value is the same for the column player and
the third is the expected payoff of the row player in the nash
equilibrium represented by the oddment vectors."
  (let ((payoff (if (listp payoff)
                    (list-to-2d-matrix payoff)
                    payoff)))
    (destructuring-bind (n-rows n-cols) (array-dimensions payoff)
      (let* ((row-cum-payoff (make-array n-rows :initial-element 0))
             (col-cum-payoff (make-array n-cols :initial-element 0))
             (colpos (make-array n-cols
                                 :initial-contents
                                 (loop for i below n-cols collect i)))
             (rowpos (make-array n-rows
                                 :initial-contents
                                 (loop for i below n-rows collect (- i))))
             (colcnt (make-array n-cols :initial-element 0))
             (rowcnt (make-array n-rows :initial-element 0))
             (active 0))
        (dotimes (i n-iterations)
          (incf (aref rowcnt active))
          (dotimes (col n-cols)
            (incf (aref col-cum-payoff col)
                  (aref payoff active col)))
          (setq active (aref colpos (nth-value 1 (extremum col-cum-payoff #'<))))
          (incf (aref colcnt active))
          (dotimes (row n-rows)
            (incf (aref row-cum-payoff row)
                  (aref payoff row active)))
          (setq active
                (- (aref rowpos (nth-value 1 (extremum row-cum-payoff #'>))))))
        (values rowcnt colcnt
                (/ (+ (extremum row-cum-payoff #'>)
                      (extremum col-cum-payoff #'<))
                   2.0
                   n-iterations))))))
