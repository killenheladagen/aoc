
(defparameter lines '(
(0 3 6 9 12 15)
(1 3 6 10 15 21)
(  10 13 16 21 30 45)))

(load "9-data.lisp")

(defun diff (a) (mapcar #'- (cdr a) a))

(defun predict (a)
  (if (every #'zerop a)
      0
      (+ (car (last a)) (predict (diff a)))))

(defparameter sum (reduce #'+ (mapcar #'predict lines)))
