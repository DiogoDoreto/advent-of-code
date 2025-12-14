(defun day9/parse-input (filepath)
  (with-open-file (input filepath)
    (do* ((result '())
          (line (read-line input nil) (read-line input nil)))
         ((null line) (nreverse result))
      (push (mapcar #'parse-integer (str:split "," line))
            result))))

(defmacro day9/loop-combinations ((a b list &optional result) &body body)
  (let ((rest-var (gensym "REST")))
    `(do* ((,a (car ,list) (car ,rest-var))
           (,rest-var (cdr ,list) (cdr ,rest-var)))
      ((null ,rest-var) ,result) (dolist (,b ,rest-var) ,@body))))

(defun day9/calc-area (c1 c2)
  (let ((x1 (car c1))
        (y1 (cadr c1))
        (x2 (car c2))
        (y2 (cadr c2)))
    (* (1+ (abs (- x2 x1)))
       (1+ (abs (- y2 y1))))))

(defun day9/solve-part1 (filepath)
  (let ((coords (day9/parse-input filepath))
        (max-area 0))
    (day9/loop-combinations (a b coords max-area)
      (let ((area (day9/calc-area a b)))
        (when (> area max-area)
          (setf max-area area))))))

(assert (= 50 (day9/solve-part1 "./day9-input0.txt")))
(assert (= 4735268538 (day9/solve-part1 "./day9-input1.txt")))

(defmacro day9/loop-seq2 ((a b list) &body body)
  (let ((rest-var (gensym "REST")))
    `(do* ((,a (car ,list) ,b)
           (,rest-var (cdr ,list) (cdr ,rest-var))
           (,b (car ,rest-var) (car ,rest-var)))
      ((null ,b) (let ((,b (car ,list))) ,@body)) ,@body)))

;; (let ((l '(1 2 3 4)))
;;   (day9/loop-seq2 (a b l)
;;     (format t "~S ~S ~%" a b)))
;; ; 1 2
;; ; 2 3
;; ; 3 4
;; ; 4 1
;; ;  => NIL

(defun day9/valid-square (hlines vlines c1 c2)
  (let* ((x1 (car c1)) (y1 (cadr c1))
         (x2 (car c2)) (y2 (cadr c2))
         (minx (min x1 x2)) (miny (min y1 y2))
         (maxx (max x1 x2)) (maxy (max y1 y2)))
    (and
     (loop for line in hlines
           for liney = (car line)
           for linex1 = (cadr line)
           for linex2 = (caddr line)
           if (and (<= (1+ miny) liney (1- maxy))
                   (or (<= linex1 (1+ minx) linex2)
                       (<= linex1 (1- maxx) linex2)))
             do (return nil)
           finally (return t))
     (loop for line in vlines
           for linex = (car line)
           for liney1 = (cadr line)
           for liney2 = (caddr line)
           if (and (<= (1+ minx) linex (1- maxx))
                   (or (<= liney1 (1+ miny) liney2)
                       (<= liney1 (1- maxy) liney2)))
             do (return nil)
           finally (return t)))))

(defun day9/draw-svg (path1 points)
  (with-open-file (stream "./day9-output.svg" :direction :output :if-exists :supersede)
    (destructuring-bind (path-points width height) (loop for p in path1
                                                         collect (apply #'format nil "~S ~S" p) into points
                                                         maximize (car p) into width
                                                         maximize (cadr p) into height
                                                         finally (return (list (str:join " L " points) width height)))
      (format stream "<svg width=\"600\" height=\"600\" viewBox=\"0 0 ~a ~a\" xmlns=\"http://www.w3.org/2000/svg\">~%" (1+ width) (1+ height))
      (format stream "<path d=\"M ~a Z\" fill=\"none\" stroke=\"blue\" stroke-width=\"100\" />~%" path-points)
      (let* ((c1 (car points))
             (c2 (cadr points))
             (c1x (car c1))
             (c1y (cadr c1))
             (c2x (car c2))
             (c2y (cadr c2))
             (x (min c1x c2x))
             (y (min c1y c2y))
             (width (- (max c1x c2x) x))
             (height (- (max c1y c2y) y)))
        (format stream "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" fill=\"none\" stroke=\"red\" stroke-width=\"100\" />~%"
                x y width height))
      (write-string "</svg>" stream))))

(defun day9/solve-part2 (filepath)
  (let ((coords (day9/parse-input filepath))
        (coords-with-area '())
        (hlines '())
        (vlines '()))
    (day9/loop-seq2 (c1 c2 coords)
      (let ((x1 (car c1)) (y1 (cadr c1))
            (x2 (car c2)) (y2 (cadr c2)))
        (when (= x1 x2)
          (push (list x1 (min y1 y2) (max y1 y2)) vlines))
        (when (= y1 y2)
          (push (list y1 (min x1 x2) (max x1 x2)) hlines))))
    (day9/loop-combinations (c1 c2 coords)
      (let ((area (day9/calc-area c1 c2)))
        (push (list area c1 c2) coords-with-area)))
    (setf coords-with-area (sort coords-with-area #'> :key #'car))
    (let ((big (sequence:find-if (lambda (c) (apply #'day9/valid-square hlines vlines c))
                                 coords-with-area
                                 :key #'cdr)))
      (day9/draw-svg coords (cdr big))
      (car big))))

(assert (= 1537458069 (day9/solve-part2 "./day9-input1.txt")))
