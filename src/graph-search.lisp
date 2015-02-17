(in-package :micmac)

(defsection @micmac-graph-search (:title "Graph Search")
  (alpha-beta function)
  (micmac.uct:@micmac-uct section))

(defun alpha-beta (state &key (depth 0) alpha beta
                   call-with-action maybe-evaluate-state list-actions
                   record-best)
  "Alpha-beta pruning for two player, zero-sum maximax (like minimax
  but both players maximize and the score is negated when passed
  between depths). Return the score of the game STATE from the point
  of view of the player to move at DEPTH and as the second value the
  list of actions of the principal variant.

  CALL-WITH-ACTION is a function of (STATE DEPTH ACTION FN). It
  carries out ACTION (returned by LIST-ACTIONS or NIL) to get the
  state corresponding to DEPTH and calls FN with that state. It may
  destructively modify STATE provided it undoes the damage after FN
  returns. CALL-WITH-ACTION is called with NIL as ACTION for the root
  of the tree, in this case STATE need not be changed. FN returns the
  same kinds of values as ALPHA-BETA. They may be useful for logging.

  MAYBE-EVALUATE-STATE is a function of (STATE DEPTH). If STATE at
  DEPTH is a terminal node then it returns the score from the point of
  view of the player to move and as the second value a list of actions
  that lead from STATE to the position that was evaluated. The list of
  actions is typically empty. If we are not at a terminal node then
  MAYBE-EVALUATE-STATE returns NIL.

  LIST-ACTIONS is a function of (STATE DEPTH) and returns a non-empty
  list of legal candidate moves for non-terminal nodes. Actions are
  tried in the order LIST-ACTIONS returns them: stronger moves

  CALL-WITH-ACTION, MAYBE-EVALUATE-STATE and LIST-ACTIONS are
  mandatory.

  RECORD-BEST, if non-NIL, is a function of (DEPTH SCORE ACTIONS). It
  is called when at DEPTH a new best action is found. ACTIONS is a
  list of all the actions in the principle variant corresonding to the
  newly found best score. RECORD-BEST is useful for graceful
  degradation in case of timeout.

  ALPHA and BETA are typically NIL (equivalent to -infinity,
  +infinity) but any real number is allowed if the range of scores can
  be boxed.

  See `test/test-alpha-beta.lisp` for an example."
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
