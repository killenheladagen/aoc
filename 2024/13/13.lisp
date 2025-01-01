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

;; (defun solve-Ax+By=C (A B C)
;;   (when (zerop (rem C (gcd A B)))
;;     (flet ((solution-p (x y)
;;              (= (+ (* A x) (* B y)) C)))
;;       (let ((max-x (floor C A)))
;;         (remove-if #'null
;;                    (loop for x from 0 to max-x collect
;;                                                (let ((y (floor (- C (* A x)) B)))
;;                                                  (when (solution-p x y)
;;                                                    (cons x y)))))))))

(defun solve-Ax+By=C (a b c)
  (assert (and (> a 0) (> b 0) (> c 0)))
  (let ((d (gcd a b)))
    (when (zerop (rem c d))
      (flet ((x (y)
               (/ (- c (* b y)) a)))
        (let ((y0 0))
          (loop while (not (integerp (x y0))) do
            (incf y0))
          (let* ((x0 (x y0))
                 (kx (/ b d))
                 (ky (/ a d))
                 (n-min (ceiling (- x0) kx))
                 (n-max (floor y0 ky)))
            (values (lambda (n)
                      (cons (+ x0 (* kx n))
                            (+ y0 (- (* ky n)))))
                    n-min n-max)))))))

;; (multiple-value-bind (f n-min n-max) (solve-Ax+By=C 94 22 8400)
;;   (print (funcall f n-min))
;;   (print (funcall f n-max)))

;; (print (list x0 y0 kx ky n-min n-max))
;; ;;(loop for n from n-min to n-max do
;; (mapc (lambda (n)
;;         (let* ((x (+ x0 (* kx n)))
;;                (y (+ y0 (- (* ky n)))))
;;           (print (list x y (+ (* a x) (* b y))))))
;;       (list n-min n-max))
;; (- (1+ n-max) n-min)))))))
;; x0 + kx*n >= 0
;; y0 - ky*n >= 0
;;
;; kx*n >= -x0
;; -ky*n >= -y0
;; ky*n <= y0
;;
;; n >= -x0/kx
;; n <= y0/ky
;; -x0/kx <= n <= y0/ky
;;
;; x = x0 + (* kx n) <= c
;; (* kx n) <= c - x0
;; n <= (c - x0) / kx

;;   (multiple-value-bind (A B C) (let ((div (gcd A B C)))
;;                                  (values (floor A div) (floor B div) (floor C div)))

(compile 'solve-Ax+By=C)

(defun solve-complex-Ax+By=C (A B C)
  (let ((solution-sets
          (mapcar (lambda (part)
                    (multiple-value-bind (f n-min n-max)
                        (apply #'solve-Ax+By=C (mapcar part (list A B C)))
                      (when f (loop for n from n-min to n-max collect (funcall f n)))))
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
;;(print (fewest-number-of-tokens "input.txt" t))
