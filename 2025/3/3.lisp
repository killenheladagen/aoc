(load "../aoc")
(require 'uiop)

(defun read-digit-file (file)
  (mapcar (lambda (line)
            (loop for c across line collect (digit-char-p c)))
          (uiop:read-file-lines file)))

(defun all-but-last (list)
  (reverse (cdr (reverse list))))

(defun largest-digit-in-list (digits min-items-left &optional (le 9))
  (assert (>= le 0))
  (let ((x (member le digits)))
    (if (> (length x) min-items-left)
        x
        (largest-digit-in-list digits min-items-left (1- le)))))

(defun largest-pair (digits)
  (print digits)
  (let* ((a (largest-digit-in-list digits 1))
         (b (largest-digit-in-list (cdr a) 0)))
    (print (+ (* 10 (car a)) (car b)))))

(defun sum-of-max-joltage (file)
  (reduce #'+
          (mapcar #'largest-pair
                  (read-digit-file file))))

(aoc (lambda (file) (sum-of-max-joltage file)) 357)
