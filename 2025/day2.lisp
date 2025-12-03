(defun char-digitp (ch)
  (char<= #\0 ch #\9))

(defun read-int (stream)
  (loop for ch = (peek-char t stream nil)
        while (and ch (char-digitp ch))
        do (read-char stream)
        collect ch into res
        finally (return (parse-integer (concatenate 'string res)))))

(defun read-int-range (stream)
  (let ((num1 (read-int stream))
        (sep (read-char stream))
        (num2 (read-int stream)))
    (list num1 num2)))

(defun process-input (sum-invalids-fn)
  (with-open-file (input "./day2-input1.txt")
    (loop for next = (peek-char t input nil)
          while next
          if (char-digitp next)
            sum (apply sum-invalids-fn (read-int-range input))
          else do (read-char input))))


;;; Part 1

(defun half-pattern (num)
  (let* ((str (write-to-string num))
         (len (length str)))
    (when (= 1 (mod len 2))
      (setf str (concatenate 'string "1" (fill str #\0))))
    (let* ((pattern-length (/ (length str) 2))
           (pattern (subseq str 0 pattern-length)))
      (parse-integer pattern))))

(assert (equal (half-pattern 99) 9))
(assert (equal (half-pattern 123) 10))
(assert (equal (half-pattern 1234) 12))

(defun sum-invalid-ids-double-pattern (from to)
  (loop for num from (half-pattern from) to (half-pattern to)
        for id = (parse-integer (format nil "~S~S" num num))
        if (<= from id to)
          sum id))

(assert (equal (sum-invalid-ids-double-pattern 11 22) 33))
(assert (equal (sum-invalid-ids-double-pattern 13 22) 22))
(assert (equal (sum-invalid-ids-double-pattern 13 21) 0))
(assert (equal (sum-invalid-ids-double-pattern 99 115) 99))
(assert (equal (sum-invalid-ids-double-pattern 998 1012) 1010))

(format t "Day 2, part 1 = ~S~%"
        (process-input #'sum-invalid-ids-double-pattern))
                                        ;  => "Day 2, part 1 = 12599655151"


;;; Part 2

(defun find-divisors (n)
  (loop for i from 1 to (floor (/ n 2))
        if (= 0 (mod n i))
          collect i))

(assert (equal (find-divisors 2) '(1)))
(assert (equal (find-divisors 3) '(1)))
(assert (equal (find-divisors 4) '(1 2)))
(assert (equal (find-divisors 9) '(1 3)))
(assert (equal (find-divisors 10) '(1 2 5)))

(defun make-min-number-with-length (n)
  (expt 10 (1- n)))

(assert (= (make-min-number-with-length 1) 1))
(assert (= (make-min-number-with-length 2) 10))
(assert (= (make-min-number-with-length 3) 100))

(defun make-max-number-with-length (n)
  (1- (make-min-number-with-length (1+ n))))

(assert (= (make-max-number-with-length 1) 9))
(assert (= (make-max-number-with-length 2) 99))
(assert (= (make-max-number-with-length 3) 999))

(defun repeat-string (str times)
  (apply #'concatenate 'string
         (loop repeat times
               collect str)))

(assert (equal (repeat-string "12" 3) "121212"))

(defun make-patterns (number-length pattern-length)
  (loop with n-repeats = (/ number-length pattern-length)
        for pattern-int from (make-min-number-with-length pattern-length)
          to (make-max-number-with-length pattern-length)
        collect (parse-integer
                 (repeat-string (write-to-string pattern-int) n-repeats))))

(assert (equal (make-patterns 2 1) '(11 22 33 44 55 66 77 88 99)))

(defun sum-invalid-ids-multi-pattern (from to)
  (let* ((from-str (write-to-string from))
         (from-str-len (length from-str))
         (to-str (write-to-string to))
         (to-str-len (length to-str))
         (collected-ids (make-hash-table :test 'equal)))
    (loop for id-len from from-str-len to to-str-len
          sum (loop for pattern-length in (find-divisors id-len)
                    sum (loop for id in (make-patterns id-len pattern-length)
                              if (and (<= from id to) (not (gethash id collected-ids)))
                                do (setf (gethash id collected-ids) t)
                                and sum id)))))

(format t "Day 2, part 2 = ~S~%"
        (process-input #'sum-invalid-ids-multi-pattern))
                                        ;  => "Day 2, part 2 = 20942028255"
