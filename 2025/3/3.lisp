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

(defun largest-jolt-list (digits jolt-len)
  (when (> jolt-len 0)
    (let* ((a (largest-digit-in-list digits (1- jolt-len))))
      (cons (car a) (largest-jolt-list (cdr a) (1- jolt-len))))))

(defun largest-jolt (digits jolt-len)
  (parse-integer (format nil "~{~a~}" (largest-jolt-list digits jolt-len))))

(defun sum-of-max-joltage (file jolt-len)
  (reduce #'+
          (mapcar (lambda (digits) (largest-jolt digits jolt-len))
                  (read-digit-file file))))

(aoc (lambda (file) (sum-of-max-joltage file 2)) 357
     (lambda (file) (sum-of-max-joltage file 12)) 3121910778619)
