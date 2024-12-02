(require 'cl-ppcre)

(defun read-file (f)
  (mapcar (lambda (line)
            (mapcar #'parse-integer (cl-ppcre:split "\\s+" line)))
          (uiop:read-file-lines f)))

(defun total-distance (f)
  (let* ((xy (read-file f))
         (x (sort (mapcar #'car xy) #'<))
         (y (sort (mapcar #'cadr xy) #'<)))
    (reduce #'+ (mapcar (lambda (a b) (abs (- a b))) x y))))

(assert (eq (total-distance "test.txt") 11))
(print (total-distance "input.txt"))

(defun total-similarity-score (f)
  (let* ((xy (read-file f))
         (x (mapcar #'car xy))
         (y (mapcar #'cadr xy)))
    (reduce #'+ (mapcar (lambda (a) (* a (count a y))) x))))

(assert (eq (total-similarity-score "test.txt") 31))
(print (total-similarity-score "input.txt"))
