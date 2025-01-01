(require 'cl-ppcre)
(require 'uiop)

(defun read-claw-file (filename add-offset)
  (let ((offset (* #C(1 1) (if add-offset 10000000000000 0)))
        (a)
        (b))
    (mapcan (lambda (row)
              (cl-ppcre:register-groups-bind (button (#'parse-integer x y)) ("Button (.+): X\\+(.+), Y\\+(.+)" row)
                (let ((c (complex x y)))
                  (if (string= button "A") (setf a c) (setf b c))))
              (cl-ppcre:register-groups-bind ((#'parse-integer x y)) ("Prize: X=(.+), Y=(.+)" row)
                (list (list a b (+ (complex x y) offset)))))
            (uiop:read-file-lines filename))))

(defun solve-Ax+By=C (A B C)
  (flet ((solution-p (x y)
           (= (+ (* A x) (* B y)) C)))
    (let ((max-x (floor C A)))
      (remove-if #'null
                 (loop for x from 0 to max-x collect
                                             (let ((y (floor (- C (* A x)) B)))
                                               (when (solution-p x y)
                                                 (cons x y))))))))

(compile 'solve-Ax+By=C)

(defun solve-complex-Ax+By=C (A B C)
  (let ((solution-sets
          (mapcar (lambda (part)
                    (apply #'solve-Ax+By=C (mapcar part (list A B C))))
                  (list #'realpart #'imagpart))))
    (intersection (car solution-sets) (cadr solution-sets) :test #'equal)))

(compile 'solve-complex-Ax+By=C)

(defun cost (ab)
  (let ((a (car ab))
        (b (cdr ab)))
    (+ (* 3 a) b)))

(compile 'cost)

(defun min-cost (x y)
  (if (< (cost x) (cost y)) x y))

(compile 'min-cost)

(defun cost-of-cheapest-solution (A B C)
  (let ((solutions (solve-complex-Ax+By=C A B C)))
    (if solutions (cost (reduce #'min-cost solutions)) 0)))

(defun fewest-number-of-tokens (filename add-offset)
  (reduce #'+ (mapcar (lambda (claw-setup)
                        (apply #'cost-of-cheapest-solution claw-setup))
                      (read-claw-file filename add-offset))))

(assert (= (fewest-number-of-tokens "test.txt" nil) 480))
(assert (= (print (fewest-number-of-tokens "input.txt" nil)) 37680))
(print (fewest-number-of-tokens "test.txt" t))
(print (fewest-number-of-tokens "input.txt" t))
