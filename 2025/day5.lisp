(defun day5/parse-fresh-ranges (input)
  (loop for line = (read-line input nil)
        until (str:emptyp line)
        collect (mapcar #'parse-integer (str:split "-" line))))

(defun day5/number-in-rangep (number range)
  (let ((min (car range))
        (max (cadr range)))
    (<= min number max)))

(defun day5/solve-part1 ()
  (with-open-file (input "./day5-input1.txt")
    (let ((ranges (day5/parse-fresh-ranges input)))
      (loop for line = (read-line input nil)
            while line
            for number = (parse-integer line)
            count (some (lambda (range) (day5/number-in-rangep number range)) ranges)))))

(day5/solve-part1)
                                        ; => 865 (10 bits, #x361)

;;; Part 2

(defun day5/merge-range (a b)
  (when (or (day5/number-in-rangep (car b) a)
            (day5/number-in-rangep (cadr b) a)
            (day5/number-in-rangep (car a) b)
            (day5/number-in-rangep (cadr a) b))
    (list (min (car a) (car b))
          (max (cadr a) (cadr b)))))

(defun day5/merge-into-ranges (new-range ranges)
  (loop for r in ranges
        for merged = (day5/merge-range new-range r)
        if merged
          do (return (day5/merge-into-ranges merged (remove r ranges)))
        finally (return (push new-range ranges))))

(defun day5/solve-part2 ()
  (with-open-file (input "./day5-input1.txt")
    (loop with merged-ranges = '()
          for line = (read-line input nil)
          until (str:emptyp line)
          for range = (mapcar #'parse-integer (str:split "-" line))
          do (setq merged-ranges (day5/merge-into-ranges range merged-ranges))
          finally (return (loop for r in merged-ranges
                                sum (1+ (- (cadr r) (car r))))))))


(day5/solve-part2)
                                        ; => 352556672963116 (49 bits, #x140A5FF41522C)
