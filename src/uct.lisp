(in-package :micmac.uct)

(defsection @micmac-uct (:title "\\UCT")
  "UCT Monte Carlo tree search. This is what makes current Go programs
  tick. And Hex programs as well, for that matter. This is a cleanup
  and generalization of code originally created in course of the
  Google AI Challenge 2010.

  For now, the documentation is just a reference. See
  `test/test-uct.lisp` for an example."
  (uct-node class)
  (depth (reader uct-node))
  (edges (accessor uct-node))
  (average-reward (accessor uct-node))
  (uct-edge class)
  (action (reader uct-edge))
  (from-node (accessor uct-edge))
  (to-node (accessor uct-edge))
  (visited-edges function)
  (unvisited-edges function)
  (edge-score generic-function)
  (select-edge generic-function)
  (outcome->reward generic-function)
  (update-uct-statistics generic-function)
  (make-uct-node generic-function)
  (state generic-function)
  (list-edges generic-function)
  (play-out generic-function)
  (uct function))

(defclass uct-node ()
  ((depth :initform 0 :initarg :depth :reader depth)
   (n-visits
    :initform 0 :accessor n-visits
    :documentation "The number of times this node was visited.")
   (edges :accessor edges :documentation "Outgoing edges.")
   (average-reward
    :initform 0 :initarg :average-reward :accessor average-reward
    :documentation "Average reward over random playouts started from
    below this node. See UPDATE-UCT-STATISTICS and REWARD."))
  (:documentation "A node in the UCT tree. Roughly translates to a
  state in the search space. Note that the state itself is not stored
  explicity, but it can be recovered by `replaying' the actions from
  the starting state or by customizing MAKE-UCT-NODE."))

(defmethod print-object ((node uct-node) stream)
  (pprint-logical-block (stream ())
    (print-unreadable-object (node stream :type t)
      (format stream "~S ~S ~S"
              (ignore-errors (depth node))
              (ignore-errors (n-visits node))
              (ignore-errors (average-reward node)))))
  node)

(defclass uct-edge ()
  ((action
    :initarg :action :reader action
    :documentation "The action represented by the edge.")
   (from-node
    :initarg :from-node :accessor from-node
    :documentation "The node this edge starts from.")
   (to-node
    :initform nil :accessor to-node
    :documentation "The node this edge points to if the edge has been
    visited or NIL.")
   (n-visits
    :initform 0 :accessor n-visits
    :documentation "The number of times this action was taken from the
    parent state."))
  (:documentation "An edge in the UCT tree. Represents an action taken
  from a state. The value of an action is the value of its target
  state which is not quite as generic as it could be; feel free to
  specialize AVERAGE-REWARD for the edges if that's not the case."))

(defmethod print-object ((edge uct-edge) stream)
  (pprint-logical-block (stream ())
    (print-unreadable-object (edge stream :type t)
      (format stream "~S ~S ~S"
              (ignore-errors (n-visits edge))
              (ignore-errors (average-reward edge))
              (ignore-errors (action edge)))))
  edge)

(defmethod average-reward ((edge uct-edge))
  (average-reward (to-node edge)))

(declaim (inline random-element))
(defun random-element (seq)
  (elt seq (random (length seq))))

(defun extremum (seq pred &key (key #'identity))
  (reduce (lambda (x y)
            (if (funcall pred (funcall key x) (funcall key y))
                x
                y))
          seq))

(defun visited-edges (node)
  (remove nil (edges node) :key #'to-node))

(defun unvisited-edges (node)
  (remove nil (edges node) :key #'to-node :test-not #'eql))

(defgeneric edge-score (node edge exploration-bias)
  (:method ((node uct-node) edge exploration-bias)
    (+ (average-reward edge)
       (* exploration-bias
          (sqrt (/ (log (n-visits node))
                   (n-visits edge)))))))

(defgeneric select-edge (node exploration-bias)
  (:documentation "Choose an action to take from a state, in other
  words an edge to follow from NODE in the tree. The default
  implementation chooses randomly from the yet unvisited edges or if
  there is none moves down the edge with the maximum EDGE-SCORE. If
  you are thinking of customizing this, for example to make it choose
  the minimum at odd depths, the you may want to consider specializing
  REWARD or UPDATE-UCT-STATISTICS instead.")
  (:method ((node uct-node) exploration-bias)
    (let ((unvisited-edges (unvisited-edges node)))
      (if unvisited-edges
          (random-element unvisited-edges)
          (extremum (edges node) #'>
                    :key (lambda (edge)
                           (edge-score node edge exploration-bias)))))))

(defgeneric outcome->reward (node outcome)
  (:documentation "Compute the reward for a node in the tree from
  OUTCOME that is the result of a playout. This is called by the
  default implementation of UPDATE-UCT-STATISTICS. This is where one
  typically negates depending on the parity of DEPTH in two player
  games.")
  (:method ((node uct-node) outcome)
    outcome))

(defgeneric update-uct-statistics (root path outcome)
  (:documentation "Increment the number of visits and update the
  average reward in nodes and edges of PATH. By default, edges simply
  get their visit counter incremented while nodes also get an update
  to AVERAGE-REWARD based on what OUTCOME->REWARD returns.")
  (:method ((node uct-node) path outcome)
    (loop for (node edge) on path by #'cddr do
          (incf (n-visits node))
          (incf (average-reward node) (/ (- (outcome->reward node outcome)
                                            (average-reward node))
                                         (n-visits node)))
          (when edge
            (incf (n-visits edge))))))

(defgeneric make-uct-node (parent edge parent-state)
  (:documentation "Create a node representing the state that EDGE
  leads to (from PARENT). Specialize this if you want to keep track of
  the state, which is not done by default as it can be expensive,
  especially in light of TAKE-ACTION mutating it. The default
  implementation simply creates an instance of the class of PARENT so
  that one can start from a subclass of UCT-NODE and be sure that that
  class is going to be used for nodes below it.")
  (:method ((parent uct-node) edge state)
    (make-instance (class-of parent)
                   :depth (1+ (depth parent)))))

(defgeneric state (node parent edge parent-state)
  (:documentation "Return the state that corresponds to NODE. This is
  not a straightforward accessor unless NODE is customized to store
  it. The rest of the parameters are provided so that one can
  reconstruct the state by taking the action of EDGE in the
  PARENT-STATE of PARENT. It's allowed to mutate PARENT-STATE and
  return it. This function must be specialized."))

(defgeneric list-edges (node state)
  (:documentation "Return a list of edges representing the possible
  actions from NODE with STATE. This function must be customized."))

(defgeneric play-out (node state reverse-path)
  (:documentation "Play a random game from NODE with STATE and return
  the outcome that's fed into UPDATE-UCT-STATISTICS. The way the
  random game is played is referred to as `default policy' and that's
  what makes or breaks UCT search. This function must be
  customized."))

(defun uct (&key root fresh-root-state exploration-bias max-n-playouts)
  "Starting from the ROOT node, search the tree expanding it one node
  for each playout. Finally return the mutated ROOT. ROOT may be the
  root node of any tree, need not be a single node with no edges.
  FRESH-ROOT-STATE is a function that returns a fresh state
  corresponding to ROOT. This state will be destroyed unless special
  care is taken in STATE."
  (loop for i below max-n-playouts do
        (let ((state (funcall fresh-root-state))
              (path (list root)))
          ;; tree policy
          (loop
           (let ((node (first path)))
             (unless (slot-boundp node 'edges)
               (setf (edges node) (list-edges node state)))
             (unless (edges node)
               ;; fixme:
               ;;(setf (n-visits node) most-positive-fixnum)
               (return))
             (let* ((edge (select-edge node exploration-bias))
                    (new-node (unless (to-node edge)
                                (setf (to-node edge)
                                      (make-uct-node node edge state)))))
               (setq state (state (to-node edge) node edge state))
               (push edge path)
               (push (to-node edge) path)
               (when new-node
                 (return)))))
          ;; default policy
          (let ((forward-path (reverse path)))
            (let ((outcome (play-out (first path) state forward-path)))
              (update-uct-statistics (first forward-path) forward-path
                                     outcome)))))
  root)
