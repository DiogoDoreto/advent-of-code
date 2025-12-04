(defun read-file-as-grid (filepath)
  (with-open-file (input filepath)
    (loop for row = (loop for value = (read-char input nil)
                          while (and value (char/= value #\Newline))
                          collect value)
          while row
          collect row)))

(defun day4/paper-rollp (value)
  (and value (char= value #\@)))

(defun day4/count-adjacent-paper-rolls (grid item-row item-col)
  (loop with deltas = '(-1 0 1)
        for drow in deltas
        for row = (ignore-errors (elt grid (+ item-row drow)))
        if row
          sum (loop for dcol in deltas
                    for neighbor = (ignore-errors (elt row (+ item-col dcol)))
                    count (and (not (= 0 drow dcol))
                               (day4/paper-rollp neighbor)))))

(defun day4/solve-part1 ()
  (loop with grid = (read-file-as-grid "./day4-input1.txt")
        for row in grid
        for row-index from 0
        sum (loop for item in row
                  for col-index from 0
                  count (and (day4/paper-rollp item)
                             (< (day4/count-adjacent-paper-rolls grid row-index col-index)
                                4)))))

(day4/solve-part1)
                                        ; => 1549 (11 bits, #x60D)

;;; Part 2

(defun day4/remove-paper-rolls (grid)
  (loop for row in grid
        for row-index from 0
        sum (loop for item in row
                  for col-index from 0
                  if (and (day4/paper-rollp item)
                          (< (day4/count-adjacent-paper-rolls grid row-index col-index)
                             4))
                    do (setf (elt row col-index) #\x)
                    and sum 1)))

(defun day4/solve-part2 ()
  (loop with grid = (read-file-as-grid "./day4-input1.txt")
        for removed-rolls = (day4/remove-paper-rolls grid)
        while (< 0 removed-rolls)
        sum removed-rolls))

(day4/solve-part2)
                                        ; => 8887 (14 bits, #x22B7)
