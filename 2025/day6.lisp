(defun day6/calc-column (&rest values)
  (let* ((values (reverse values))
         (operator (find-symbol (car values)))
         (numbers (mapcar #'parse-integer (cdr values)))
         (cmd (cons operator numbers)))
    (eval cmd)))

(defun day6/solve-part1 ()
  (with-open-file (input "./day6-input1.txt")
    (loop for line = (read-line input nil)
          while line
          collect (str:words line) into data
          finally (return (apply #'+ (apply #'mapcar #'day6/calc-column data))))))

(day6/solve-part1)
                                        ; => 5171061464548 (43 bits, #x4B3FB481DE4)

;;; Part 2

(defun day6/read-all-lines (stream)
  (loop for line = (read-line stream nil)
        while line
        collect line))

(defun day6/solve-part2 ()
  (with-open-file (input "day6-input1.txt")
    (loop with lines = (day6/read-all-lines input)
          with numbers = '()
          with operator = nil
          for column = (apply #'str:concat (mapcar #'str:s-first lines)) ; read column as string
          do (setf lines (mapcar #'str:s-rest lines)) ; advance by removing 1st column from `lines'
          if (str:blankp column)
            sum (eval (cons operator numbers)) into result
            and do (progn
                     (when (str:emptyp column)
                       (return result))
                     (setf numbers '()
                           operator nil))
          else do (progn
                    (push (parse-integer column :junk-allowed t) numbers)
                    (unless operator
                      (setf operator (find-symbol (str:s-last column))))))))

(day6/solve-part2)
                                        ; => 10189959087258 (44 bits, #x94488E4449A)
