(in-package :micmac)

(defun test-beam-search ()
  (flet ((foo (beam-width)
           (coerce
            (beam-search
             '("") :max-depth 10 :n-solutions 4 :beam-width beam-width
             :expand-node-fn (lambda (entry)
                               (loop for i below 2
                                     collect (format nil "~A~A" entry
                                                     (code-char
                                                      (+ (char-code #\a) i)))))
             :score-fn (lambda (entry)
                         (let* ((n-a (count #\a entry))
                                (first-b-pos (position #\b entry))
                                (n-a-before-b
                                  (if first-b-pos
                                      (count #\a entry :end first-b-pos)
                                      0)))
                           (- (* 2 n-a-before-b) n-a)))
             :upper-bound-fn (lambda (entry)
                               (- 5 (count #\b entry :test-not #'eql)))
             :solutionp-fn (constantly t)
             :finishedp-fn (lambda (entry) (= 5 (length entry))))
            'list)))
    (assert (equal (foo 1) '("" "b" "bb" "bbb")))
    (assert (equal (foo 2) '("ab" "abb" "abbb" "abbbb")))
    (assert (equal (foo 2) '("ab" "abb" "abbb" "abbbb")))
    (assert (equal (foo 4) '("aab" "aabb" "aabbb" "ab")))))

(defun test-parallel-beam-search ()
  (flet ((foo (beam-width)
           (let ((solution-seqs
                   (parallel-beam-search
                    '(("") ("x")) :max-depth 10 :n-solutions 4
                    :beam-width beam-width
                    :expand-node-fn
                    (lambda (entry)
                      (loop for i below 2
                            collect (format nil "~A~A" entry
                                            (code-char
                                             (+ (char-code #\a) i)))))
                    :score-fn (lambda (entry)
                                (let* ((n-a (count #\a entry))
                                       (first-b-pos (position #\b entry))
                                       (n-a-before-b
                                         (if first-b-pos
                                             (count #\a entry :end first-b-pos)
                                             0)))
                                  (- (* 2 n-a-before-b) n-a)))
                    :upper-bound-fn (lambda (entry)
                                      (- 5 (count #\b entry :test-not #'eql)))
                    :solutionp-fn (constantly t)
                    :finishedp-fn (lambda (entry) (= 5 (length entry))))))
             (map 'list (lambda (solution-seq)
                          (coerce solution-seq 'list))
                  solution-seqs))))
    (assert (equal (foo 1) '(("" "b" "bb" "bbb")
                             ("x" "xb" "xbb" "xbbb"))))
    (assert (equal (foo 2) '(("ab" "abb" "abbb" "abbbb")
                             ("xab" "xabb" "xabbb" "x"))))
    (assert (equal (foo 2) '(("ab" "abb" "abbb" "abbbb")
                             ("xab" "xabb" "xabbb" "x"))))
    (assert (equal (foo 4) '(("aab" "aabb" "aabbb" "ab")
                             ("xaab" "xaabb" "xab" "xabb"))))))
