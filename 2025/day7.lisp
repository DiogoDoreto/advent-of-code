(defun day7/solve-part1 (filepath)
  (with-open-file (input filepath)
    (do* ((active-beams (list (position #\S (read-line input))))
          (split-count 0)
          (line (read-line input nil) (read-line input nil)))
         ((null line) split-count)
      (dolist (beam-index active-beams)
        (when (char= #\^ (aref line beam-index))
          (setf active-beams (remove beam-index active-beams))
          (incf split-count)
          (dolist (new-beam (list (1- beam-index) (1+ beam-index)))
            (unless (find new-beam active-beams)
              (push new-beam active-beams))))))))

(assert (= 21 (day7/solve-part1 "./day7-input0.txt")))
(assert (= 1605 (day7/solve-part1 "./day7-input1.txt")))

(defparameter day7/*lines* nil)
(defparameter day7/*cache* nil)

(defmacro day7/with-cached-value ((cache key) &body body)
  "Macro for memoization: evaluates KEY and checks CACHE for an existing value.
If present, returns the cached value. Otherwise, evaluates BODY, caches the result
under KEY in CACHE, and returns it. Assumes CACHE is a hash table with appropriate test."
  (let ((result-var (gensym "RESULT"))
        (present-p-var (gensym "PRESENT-P")))
    `(multiple-value-bind (,result-var ,present-p-var) (gethash ,key ,cache)
       (if ,present-p-var ,result-var
           (let ((,result-var (progn ,@body)))
             (setf (gethash ,key ,cache) ,result-var)
             ,result-var)))))

(defun day7/run-timeline (beam-index row-index)
  (day7/with-cached-value (day7/*cache* (cons beam-index row-index))
    (if (>= row-index (length day7/*lines*))
        1
        (if (char/= #\^ (aref (elt day7/*lines* row-index) beam-index))
            (day7/run-timeline beam-index (1+ row-index))
            (+ (day7/run-timeline (1- beam-index) (1+ row-index))
               (day7/run-timeline (1+ beam-index) (1+ row-index)))))))

(defun day7/read-lines (stream)
  (do* ((lines nil)
        (line (read-line stream nil) (read-line stream nil)))
       ((null line) (nreverse lines))
    (push line lines)))

(defun day7/solve-part2 (filepath)
  (with-open-file (input filepath)
    (let ((beam-index (position #\S (read-line input))))
      (setf day7/*lines* (day7/read-lines input)
            day7/*cache* (make-hash-table :test 'equal))
      (day7/run-timeline beam-index 0))))

(assert (= 40 (day7/solve-part2 "./day7-input0.txt")))
(assert (= 29893386035180 (day7/solve-part2 "./day7-input1.txt")))
