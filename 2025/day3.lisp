(defun char-digitp (ch)
  (char<= #\0 ch #\9))

(defun read-digits (stream)
  (loop for ch = (peek-char nil stream nil)
        while (and ch (char-digitp ch))
        do (read-char stream)
        collect ch into res
        finally (return (concatenate 'string res))))

;;; Part 1

(defun max-bank-joltage-2 (bank)
  (loop with len = (length bank)
        for i from 0 below len
        maximize (loop for j from (1+ i) below len
                       for value = (parse-integer (concatenate 'string (list (aref bank i) (aref bank j))))
                       maximize value)))

(assert (= (max-bank-joltage-2 "987654321111111") 98))
(assert (= (max-bank-joltage-2 "811111111111119") 89))
(assert (= (max-bank-joltage-2 "234234234234278") 78))
(assert (= (max-bank-joltage-2 "818181911112111") 92))

(defun day3-part1 ()
  (with-open-file (input "./day3-input1.txt")
    (loop for next = (peek-char t input nil)
          while next
          if (char-digitp next)
            sum (max-bank-joltage-2 (read-digits input))
          else do (read-char input))))

(format t "Day 3, part 1: ~S~%" (day3-part1))
                                        ; Day 3, part 1: 17766

;;; Part 2

(defun max-bank-joltage-n (bank nbatteries)
  (let ((len (length bank))
        (max-value (aref bank 0))
        (max-index 0))
    (loop for i from 1 below (- len (1- nbatteries))
          for current = (aref bank i)
          if (char< max-value current)
            do (setf max-value current)
            and do (setf max-index i))
    (if (= nbatteries 1)
        (list max-value)
        (concatenate 'string (list max-value)
                     (max-bank-joltage-n
                      (subseq bank (1+ max-index))
                      (1- nbatteries))))))

(defun day3-part2 ()
  (with-open-file (input "./day3-input1.txt")
    (loop for next = (peek-char t input nil)
          while next
          if (char-digitp next)
            sum (parse-integer (max-bank-joltage-n (read-digits input) 12))
          else do (read-char input))))

(format t "Day 3, part 2: ~S~%" (day3-part2))
                                        ; Day 3, part 2: 176582889354075
