(require 'cl-ppcre)

(defun read-file (f)
  (mapcar (lambda (line)
            (mapcar #'parse-integer (cl-ppcre:split "\\s+" line)))
          (uiop:read-file-lines f)))

(defun safe-p (x)
  (let* ((diffs (mapcar #'- x (cdr x)))
         (max (reduce #'max diffs))
         (min (reduce #'min diffs)))
    (or (and (>= min 1) (<= max 3))
        (and (>= min -3) (<= max -1)))))

(defun safe-count (f)
  (count-if #'safe-p (read-file f)))

(assert (eq (safe-count "test.txt") 2))
(print (safe-count "input.txt"))

(defun remove-nth (n list)
  (loop for x in list
        for i from 0
        unless (= i n) collect x))

(defun problem-damp-safe-p (x &optional n)
  (if (not n)
      (problem-damp-safe-p x (1+ (length x)))
      (when (>= n 0) (or (safe-p (remove-nth n x))
                         (problem-damp-safe-p x (1- n))))))

(defun problem-dampener-safe-count (f)
  (count-if #'problem-damp-safe-p (read-file f)))

(assert (eq (problem-dampener-safe-count "test.txt") 4))
(print (problem-dampener-safe-count "input.txt"))
