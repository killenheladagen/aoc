(require 'cl-ppcre)
(require 'uiop)

(defun read-equation-file (f)
  (mapcar (lambda (line)
            (mapcar #'parse-integer (cl-ppcre:split ":?\\s+" line)))
          (uiop:read-file-lines f)))

(defun concat-nums (a b)
  (parse-integer (format nil "~a~a" a b)))

(defparameter *use-concat* nil)

(defun solvable-equation-p (lhs acc expr)
  ;;(format t "~a = ~a ? ~a~%" lhs acc expr)
  (unless (> acc lhs)
    (if (null expr)
        (= lhs acc)
        (or (solvable-equation-p lhs (* acc (car expr)) (cdr expr))
            (solvable-equation-p lhs (+ acc (car expr)) (cdr expr))
            (and *use-concat* (solvable-equation-p lhs (concat-nums acc (car expr)) (cdr expr)))))))

(defun solve (equation)
  (when (solvable-equation-p (car equation) 0 (cdr equation))
    equation))

(defun solvable-equations (equations)
  (remove-if #'null (mapcar #'solve equations)))

(defun sum-of-solvable-equations (f)
  (let* ((all-eqs (read-equation-file f))
         (solvable-eqs (solvable-equations all-eqs)))
    (format t "~a of ~a are solvable~%" (length solvable-eqs) (length all-eqs))
    (reduce #'+ (mapcar #'car solvable-eqs))))

(assert (= (sum-of-solvable-equations "test.txt") 3749))
(print (sum-of-solvable-equations "input.txt"))

(setf *use-concat* t)
(assert (= (sum-of-solvable-equations "test.txt") 11387))
(print (sum-of-solvable-equations "input.txt"))
