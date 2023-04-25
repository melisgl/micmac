(in-package :micmac)

(defsection @micmac-graph-search (:title "Graph Search")
  (alpha-beta function)
  (beam-search function)
  (parallel-beam-search function)
  (micmac.uct::@micmac-uct section))

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

(defun beam-search (start-nodes &key max-depth (n-solutions 1)
                    (beam-width (length start-nodes))
                    expand-node-fn expand-beam-fn score-fn upper-bound-fn
                    solutionp-fn (finishedp-fn solutionp-fn))
  "In a graph, search for nodes that with the best scores with [beam
  search](http://en.wikipedia.org/wiki/Beam_search). That is, starting
  from START-NODES perform a breadth-first search but at each depth
  only keep BEAM-WIDTH number of nodes with the best scores. Keep the
  best N-SOLUTIONS (at most) complete solutions. Discard nodes known
  to be unable to get into the best N-SOLUTIONS (due to
  UPPER-BOUND-FN). Finally, return the solutions and the active
  nodes (the _beam_) as adjustable arrays sorted by score in
  descending order.

  START-NODES (a sequence of elements of arbitrary type). SCORE-FN,
  UPPER-BOUND-FN, SOLUTIONP-FN, FINISHEDP-FN are all functions of one
  argument: the node. SOLUTIONP-FN checks whether a node represents a
  complete solution (i.e. some goal is reached). SCORE-FN returns a
  real number that's to be maximized, it's only called for node for
  which SOLUTIONP-FN returned true. UPPER-BOUND-FN (if not NIL)
  returns a real number that equal or greater than the score of all
  solutions reachable from that node. FINISHEDP-FN returns true iff
  there is nowhere to go from the node.

  EXPAND-NODE-FN is also a function of a single node argument. It
  returns a sequence of nodes to 'one step away' from its argument
  node. EXPAND-BEAM-FN is similar, but it takes a vector of nodes and
  returns all nodes one step away from any of them. It's enough
  provide either EXPAND-NODE-FN or EXPAND-BEAM-FN. The purpose of
  EXPAND-BEAM-FN. is to allow more efficient, batch-like operations.

  See `test/test-beam-search.lisp` for an example."
  (let ((expand-beam-fn (or expand-beam-fn
                            (lambda (nodes)
                              (loop for node across nodes
                                    append (funcall expand-node-fn node)))))
        (solutions (make-array n-solutions :adjustable t :fill-pointer 0))
        (beam (make-array beam-width :adjustable t :fill-pointer 0)))
    (flet ((has-a-chance-p (node)
             (or (null upper-bound-fn)
                 (or (< (length solutions) n-solutions)
                     (< (funcall score-fn (aref solutions (1- n-solutions)))
                        (funcall upper-bound-fn node))))))
      (loop
        for depth upfrom 0
        do (when (and max-depth (<= max-depth depth))
             (return))
           (let ((next-nodes (if (zerop depth)
                                 start-nodes
                                 (funcall expand-beam-fn beam))))
             (setf (fill-pointer beam) 0)
             (map nil (lambda (next-node)
                        (when (funcall solutionp-fn next-node)
                          (insert-into-sorted-vector next-node solutions #'>
                                                     :key score-fn))
                        (when (and (not (funcall finishedp-fn next-node))
                                   (has-a-chance-p next-node))
                          (insert-into-sorted-vector next-node beam #'>
                                                     :key score-fn)))
                  next-nodes)
             (when (zerop (length beam))
               (return)))))
    (values solutions beam)))

(defmacro dup (n maker)
  `(coerce (loop repeat ,n collect ,maker) 'vector))

(defun parallel-beam-search (start-node-seqs &key
                             max-depth (n-solutions 1) beam-width
                             expand-node-fn expand-beams-fn
                             score-fn upper-bound-fn solutionp-fn
                             (finishedp-fn solutionp-fn))
  "This is very much like BEAM-SEARCH except it solves a number of
  instances of the same search problem starting from different sets of
  nodes. The sole purpose of PARALLEL-BEAM-SEARCH is to amortize the
  cost EXPAND-BEAM-FN if possible.

  EXPAND-BEAMS-FN is called with sequence of beams (i.e. it's a
  sequence of sequence of nodes) and it must return another sequence
  of sequences of nodes. Each element of the returned sequence is the
  reachable nodes of the nodes in the corresponding element of its
  argument sequence.

  PARALLEL-BEAM-SEARCH returns a sequence of solutions sequences, and
  a sequence of active node sequences.

  See `test/test-beam-search.lisp` for an example."
  (check-type beam-width unsigned-byte)
  (let* ((expand-beams-fn
           (or expand-beams-fn
               (lambda (node-seqs)
                 (loop for nodes across node-seqs
                       collect (loop for node across nodes
                                     append (funcall expand-node-fn node))))))
         (n-searches (length start-node-seqs))
         (solution-seqs
           (dup n-searches
                (make-array n-solutions :adjustable t :fill-pointer 0)))
         (beams
           (dup n-searches
                (make-array beam-width :adjustable t :fill-pointer 0))))
    (flet ((has-a-chance-p (node solutions)
             (or (null upper-bound-fn)
                 (< (length solutions) n-solutions)
                 (< (funcall score-fn (aref solutions (1- n-solutions)))
                    (funcall upper-bound-fn node)))))
      (loop
        for depth upfrom 0
        do (when (and max-depth (<= max-depth depth))
             (return))
           (let ((next-node-seqs
                   (if (zerop depth)
                       start-node-seqs
                       (funcall expand-beams-fn beams)))
                 (emptyp t))
             (map nil
                  (lambda (beam next-nodes solutions)
                    (setf (fill-pointer beam) 0)
                    (map nil (lambda (next-node)
                               (when (funcall solutionp-fn next-node)
                                 (insert-into-sorted-vector
                                  next-node solutions #'> :key score-fn))
                               (when (and (not (funcall finishedp-fn next-node))
                                          (has-a-chance-p next-node solutions))
                                 (insert-into-sorted-vector
                                  next-node beam #'> :key score-fn)
                                 (setq emptyp nil)))
                         next-nodes))
                  beams next-node-seqs solution-seqs)
             (when emptyp
               (return)))))
    (values solution-seqs beams)))

(defmacro apply-key (key object)
  (alexandria:once-only (key object)
    `(if ,key (funcall ,key ,object) ,object)))

(defun insert-into-sorted-vector
    (item vector pred &key key (max-length (array-total-size vector)))
  "Insert ITEM into VECTOR while keeping it sorted by PRED. Extend the
  vector if needed while respecting MAX-LENGTH. "
  (declare (type (array * (*)) vector))
  (let* ((key (if key (coerce key 'function) nil))
         (pred (coerce pred 'function))
         (len (length vector))
         (item-key (apply-key key item)))
    ;; Pick off the common case quickly where ITEM won't be collected.
    (unless (and (= len max-length)
                 (funcall pred (apply-key key (aref vector (1- len))) item-key))
      (let ((pos (1+ (or (position item-key vector
                                   :key key :test-not pred :from-end t)
                         -1))))
        (when (< pos max-length)
          (when (< len max-length)
            (vector-push-extend nil vector))
          (replace vector vector :start1 (1+ pos) :start2 pos :end2 len)
          (setf (aref vector pos) item))))
    vector))
