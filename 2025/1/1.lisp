(load "../aoc")
(require 'uiop)

(defun read-rotation-file (file)
  (mapcar (lambda (line)
            (* (if (eq (char line 0) #\L) -1 1)
               (parse-integer (subseq line 1))))
          (uiop:read-file-lines file)))

(defun apply-rotation (start ops)
  (let ((x start))
    (flet ((f (a) (setq x (mod (+ x a) 100))))
      (cons start (mapcar #'f ops)))))

(defun num-stops-at-zero (start file &optional (transform #'identity))
  (let ((lst (apply-rotation start (funcall transform (read-rotation-file file)))))
    (length (remove-if-not #'zerop lst))))

(defun sign (x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        (t 0)))

(defun expand-rotations (ops)
  (mapcan (lambda (x) (make-list (abs x) :initial-element (sign x))) ops))

(aoc (lambda (file) (num-stops-at-zero 50 file)) 3
     (lambda (file) (num-stops-at-zero 50 file #'expand-rotations)) 6)
