(load "24-hail")

(defun strip-z (hail)
  (let ((pos (car hail))
	(dir (cadr hail)))
    (list (list (car pos) (cadr pos))
	  (list (car dir) (cadr dir)))))

(defun strip-z-from-input (input-block)
  (append (list (car input-block))
	  (mapcar #'strip-z (cdr input-block))))

(defun vect-len (a)
  (sqrt (reduce #'+ (mapcar (lambda (x) (* x x)) a))))

(defun norm-vect (a)
  (let ((len (vect-len a)))
    (mapcar (lambda (x) (/ x len)) a)))

(defun norm-dir (a)
  (norm-vect (cadr a)))

;;(defun direction (
(defun parallel (a b)
  (equal (norm-dir a) (norm-dir b)))

(assert (parallel '((18 19 22)(-1 -1 -2)) '((20 25 34)(-2 -2 -4))))
(assert (not (parallel '((18 19 22)(-1 -1 -2)) '((20 25 34)(-2 -3 -4)))))
