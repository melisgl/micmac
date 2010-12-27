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

(defun alpha-beta (state &key (depth 0) alpha beta
                   call-with-action maybe-evaluate-state list-actions
                   record-best)
  "Alpha-beta pruning for two player, zero-sum maximax (like minimax
but both players maximize and the score is negated when passed between
depths). Return the score of the game STATE from the point of view of
the player to move at DEPTH and as the second value the list of
actions of the principal variant.

CALL-WITH-ACTION is a function of (STATE DEPTH ACTION FN). It carries
out ACTION (returned by LIST-ACTIONS or NIL) to get the state
corresponding to DEPTH and calls FN with that state. It may
destructively modify STATE provided it undoes the damage after FN
returns. CALL-WITH-ACTION is called with NIL as ACTION for the root of
the tree, in this case STATE need not be changed. FN returns the same
kinds of values as ALPHA-BETA. They may be useful for logging.

MAYBE-EVALUATE-STATE is a function of (STATE DEPTH). If STATE at DEPTH
is a terminal node then it returns the score from the point of view of
the player to move and as the second value a list of actions that lead
from STATE to the position that was evaluated. The list of actions is
typically empty. If we are not at a terminal node then
MAYBE-EVALUATE-STATE returns NIL.

LIST-ACTIONS is a function of (STATE DEPTH) and returns a non-empty
list of legal candidate moves for non-terminal nodes. Actions are
tried in the order LIST-ACTIONS returns them: stronger moves 

CALL-WITH-ACTION, MAYBE-EVALUATE-STATE and LIST-ACTIONS are mandatory.

RECORD-BEST, if non-NIL, is a function of (DEPTH SCORE ACTIONS). It is
called when at DEPTH a new best action is found. ACTIONS is a list of
all the actions in the principle variant corresonding to the newly
found best score. RECORD-BEST is useful for graceful degradation in
case of timeout.

ALPHA and BETA are typically NIL (equivalent to -infinity, +infinity)
but any real number is allowed if the range of scores can be boxed."
  (labels
      ((foo (state depth action alpha beta)
         (funcall
          call-with-action
          state depth action
          (lambda (state)
            (multiple-value-bind (score actions)
                (multiple-value-bind (evaluation-score evaluation-actions)
                    (funcall maybe-evaluate-state state depth)
                  (cond (evaluation-score
                         (values evaluation-score evaluation-actions))
                        (t
                         (let ((max nil)
                               (max-actions nil)
                               (actions (funcall list-actions state depth)))
                           (unless actions
                             (error "No actions for non-terminal state ~S ~
                                 at depth ~S"
                                    state depth))
                           (dolist (action actions (values max max-actions))
                             (multiple-value-bind (score actions)
                                 (foo state (1+ depth) action
                                      (if beta (- beta) nil)
                                      (if alpha (- alpha) nil))
                               (when (or (null alpha) (< alpha (- score)))
                                 (setq alpha (- score)))
                               (when (or (null max) (< max (- score)))
                                 (setq max (- score))
                                 (setq max-actions (cons action actions))
                                 (when record-best
                                   (funcall record-best depth max max-actions)))
                               (when (and alpha beta (<= beta alpha))
                                 (return (values alpha :alpha-beta-cut)))))))))
              (values score actions))))))
    (foo state depth nil alpha beta)))
