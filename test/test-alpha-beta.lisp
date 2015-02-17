(in-package :micmac)

;;;; Alpha-beta tic-tac-toe example

(defun player-to-move (depth)
  (if (zerop (mod depth 2))
      #\x
      #\o))

(defparameter *directions*
  `((1 0)
    (1 1)
    (0 1)
    (-1 1)))

(defun write-board (board &optional (stream *standard-output*))
  (destructuring-bind (height width) (array-dimensions board)
    (dotimes (y height)
      (dotimes (x width)
        (write-char (or (aref board y x) #\.) stream))
      (terpri stream))))

(defun map-board-line (board x y dx dy fn)
  (destructuring-bind (height width) (array-dimensions board)
    (labels ((validp (x y)
               (and (<= 0 x) (< x width)
                    (<= 0 y) (< y height)))
             (foo (i)
               (let ((x* (+ x (* i dx)))
                     (y* (+ y (* i dy))))
                 (cond ((validp x* y*)
                        (funcall fn x* y*)
                        t)
                       (t
                        nil)))))
      (loop for i upfrom 0 while (foo i))
      (loop for i downfrom -1 while (foo i)))))

(defun winner-on-line (board x y dx dy)
  (let ((marks ()))
    (map-board-line board x y dx dy
                    (lambda (x* y*)
                      (push (aref board y* x*) marks)))
    (if (and (= 3 (length marks))
             (= 1 (length (remove-duplicates marks))))
        (first marks)
        nil)))

(defun winner (board)
  (destructuring-bind (height width) (array-dimensions board)
    (dotimes (y height)
      (dotimes (x width)
        (loop for (dx dy) in *directions* do
              (let ((winner (winner-on-line board x y dx dy)))
                (when winner
                  (return-from winner winner))))))))

(defun board-full-p (board)
  (dotimes (i (array-total-size board))
    (when (null (row-major-aref board i))
      (return-from board-full-p nil)))
  t)

(defun maybe-evaluate-state (board depth)
  (let ((winner (winner board)))
    (cond (winner
           (if (eql winner (player-to-move depth))
               ;; prefer winning quick
               (- 1 (* 0.0001 depth))
               ;; try to delay losing
               (+ -1 (* 0.0001 depth))))
          ((board-full-p board) 0)
          (t
           nil))))

(defun list-actions (board depth)
  (declare (ignore depth))
  (destructuring-bind (height width) (array-dimensions board)
    (let ((actions ()))
      (dotimes (y height)
        (dotimes (x width)
          (unless (aref board y x)
            (push (cons x y) actions))))
      actions)))

(defun call-with-action (board depth action fn)
  (labels ((stars ()
             (loop repeat (1+ depth) do (format *trace-output* "*")))
           ;; The logged game tree is easily folded/unfolded in
           ;; org-mode.
           (foo (board)
             (stars)
             (if action
                 (format *trace-output* " playing ~S~%" action)
                 (format *trace-output* " initial state~%"))
             (stars)
             (format *trace-output* "* board~%")
             (write-board board *trace-output*)
             (multiple-value-bind (score actions)
                 (funcall fn board)
               (stars)
               (format *trace-output* " score=~S actions=~S~%" score actions)
               (values score actions))))
    (if action
        (destructuring-bind (x . y) action
          (setf (aref board y x) (player-to-move (1- depth)))
          (unwind-protect
               (foo board)
            (setf (aref board y x) nil)))
        (foo board))))

(defun test-tic-tac-toe-good-defense ()
  (let ((board (make-array '(3 3) :initial-element nil)))
    (multiple-value-bind (score actions)
        (alpha-beta board
                    :call-with-action #'call-with-action
                    :maybe-evaluate-state #'maybe-evaluate-state
                    :list-actions #'list-actions)
      ;; it's a draw in nine moves
      (assert (= 0 score))
      (assert (= 9 (length actions))))))

(defun test-tic-tac-toe-bad-defense ()
  (let ((board (make-array '(3 3) :initial-contents '((nil #\o nil)
                                                      (nil #\x nil)
                                                      (nil nil nil)))))
    (multiple-value-bind (score actions)
        (alpha-beta board
                    :call-with-action #'call-with-action
                    :maybe-evaluate-state #'maybe-evaluate-state
                    :list-actions #'list-actions)
      ;; #\x wins
      (assert (< 0.9 score))
      (assert (= 5 (length actions))))))

(defun test-alpha-beta ()
  (test-tic-tac-toe-good-defense)
  (test-tic-tac-toe-bad-defense))
